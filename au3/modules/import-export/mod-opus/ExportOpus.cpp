/**********************************************************************

   SPDX-License-Identifier: GPL-2.0-or-later

   Audacity: A Digital Audio Editor

   ExportOpus.cpp

   Dmitry Vedenko

**********************************************************************/

#include "Export.h"

#include <random>
#include <string_view>

#include <ogg/ogg.h>
#include <opus/opus.h>
#include <opus/opus_multistream.h>

#include "wxFileNameWrapper.h"
#include "Mix.h"

#include "MemoryX.h"

#include "Track.h"
#include "Tags.h"

#include "ExportPluginHelpers.h"
#include "ExportOptionsEditor.h"
#include "ExportPluginRegistry.h"

#include "PlainExportOptionsEditor.h"

#include "CodeConversions.h"

namespace {
TranslatableString GetOpusEncErrorString(int error)
{
    switch (error) {
    case OPUS_OK:
        return XO("no error");
    case OPUS_BAD_ARG:
        return XO("invalid argument");
    case OPUS_BUFFER_TOO_SMALL:
        return XO("buffer too small");
    case OPUS_INTERNAL_ERROR:
        return XO("internal error");
    case OPUS_INVALID_PACKET:
        return XO("invalid packet");
    case OPUS_UNIMPLEMENTED:
        return XO("not implemented");
    case OPUS_INVALID_STATE:
        return XO("invalid state");
    case OPUS_ALLOC_FAIL:
        return XO("memory allocation has failed");
    default:
        return XO("Unknown error");
    }
}

[[noreturn]] void FailExport(const TranslatableString& title, int errorCode = 0)
{
    if (errorCode != 0) {
        throw ExportException(Verbatim("%s: %s")
                              .Format(title, GetOpusEncErrorString(errorCode))
                              .Translation());
    }

    throw ExportException(title.Translation());
}

/* i18n-hint: kbps abbreviates "thousands of bits per second" */
TranslatableString n_kbps(int n)
{
    return XO("%d kbps").Format(n);
}

enum : int
{
    OPUSOptionIDBitRate = 0,
    OPUSOptionIDQuality,
    OPUSOptionIDFrameDuration,
    OPUSOptionIDVBRMode,
    OPUSOptionIDApplication,
    OPUSOptionIDCutoff
};

namespace VBRMode {
enum : int
{
    CBR,
    VBR,
    CVBR
};
}

const std::initializer_list<PlainExportOptionsEditor::OptionDesc> OPUSOptions {
    {
        {
            OPUSOptionIDBitRate, XO("Bit Rate"),
            OPUS_AUTO,
            ExportOption::TypeEnum,
            {
                6000,
                8000,
                16000,
                24000,
                32000,
                40000,
                48000,
                64000,
                80000,
                96000,
                128000,
                160000,
                192000,
                256000,
                OPUS_AUTO,
                OPUS_BITRATE_MAX,
            },
            {
                n_kbps(6),
                n_kbps(8),
                n_kbps(16),
                n_kbps(24),
                n_kbps(32),
                n_kbps(40),
                n_kbps(48),
                n_kbps(64),
                n_kbps(80),
                n_kbps(96),
                n_kbps(128),
                n_kbps(160),
                n_kbps(192),
                n_kbps(256),
                XO("Auto"),
                XO("Maximum")
            }
        }, wxT("/FileFormats/OPUS/Bitrate")
    },
    {
        {
            OPUSOptionIDQuality, XO("Quality"),
            10,
            ExportOption::TypeRange,
            { 0, 10 }
        }, wxT("/FileFormats/OPUS/Quality")
    },
    {
        {
            OPUSOptionIDFrameDuration, XO("Frame Duration"),
            200,
            ExportOption::TypeEnum,
            {
                25,
                50,
                100,
                200,
                400,
                600,
            },
            {
                XO("2.5 ms"),
                XO("5 ms"),
                XO("10 ms"),
                XO("20 ms"),
                XO("40 ms"),
                XO("60 ms"),
            }
        }, wxT("/FileFormats/OPUS/FrameDuration")
    },
    {
        {
            OPUSOptionIDVBRMode, XO("VBR Mode"),
            VBRMode::VBR,
            ExportOption::TypeEnum,
            { VBRMode::CBR, VBRMode::VBR, VBRMode::CVBR },
            { XO("Off"), XO("On"), XO("Constrained") }
        }, wxT("/FileFormats/OPUS/VbrMode")
    },
    {
        {
            OPUSOptionIDApplication, XO("Optimize for"),
            OPUS_APPLICATION_AUDIO,
            ExportOption::TypeEnum,
            { OPUS_APPLICATION_VOIP, OPUS_APPLICATION_AUDIO, OPUS_APPLICATION_RESTRICTED_LOWDELAY },
            { XO("Speech"), XO("Audio"), XO("Low Delay") }
        }, wxT("/FileFormats/OPUS/Application")
    },
    {
        {
            OPUSOptionIDCutoff, XO("Cutoff"),
            OPUS_AUTO,
            ExportOption::TypeEnum,
            {
                OPUS_AUTO,
                OPUS_BANDWIDTH_NARROWBAND,
                OPUS_BANDWIDTH_MEDIUMBAND,
                OPUS_BANDWIDTH_WIDEBAND,
                OPUS_BANDWIDTH_SUPERWIDEBAND,
                OPUS_BANDWIDTH_FULLBAND,
            },
            {
                XO("Auto"),
                XO("Narrowband"),
                XO("Mediumband"),
                XO("Wideband"),
                XO("Super Wideband"),
                XO("Fullband")
            }
        }, wxT("/FileFormats/OPUS/Cutoff")
    },
};

constexpr int supportedSampleRates[] = { 8000, 12000, 16000, 24000, 48000 };

bool IsValidSampleRate(int sampleRate) noexcept
{
    for (auto sr : supportedSampleRates) {
        if (sr == sampleRate) {
            return true;
        }
    }
    return false;
}
}

class OpusExportProcessor final : public ExportProcessor
{
    struct OggPacket final
    {
        using byte_type = std::remove_pointer_t<decltype(ogg_packet::packet)>;
        static_assert(sizeof(byte_type) == 1);

        OggPacket(int64_t packetNo, bool resizable)
            : resizable{resizable}
        {
            packet.packetno = packetNo;
        }

        explicit OggPacket(int64_t packetNo)
            : OggPacket {packetNo, false}
        {
        }

        OggPacket(int64_t packetNo, long size, bool resizable)
            : OggPacket {packetNo, resizable}
        {
            Resize(size);
        }

        void Resize(long size)
        {
            buffer.resize(size);
            packet.packet = buffer.data();
        }

        void Reset() noexcept
        {
            packet.bytes = 0;
        }

        void MarkBOS() noexcept
        {
            packet.b_o_s = 1;
        }

        void MarkEOS() noexcept
        {
            packet.e_o_s = 1;
        }

        void Write(const void* data, const long length)
        {
            const auto nextPos = packet.bytes + length;

            if (nextPos > buffer.size()) {
                if (resizable) {
                    Resize(std::max<size_t>(1024, buffer.size() * 2));
                } else {
                    FailExport(
                        XO("Buffer overflow in OGG packet"), OPUS_BUFFER_TOO_SMALL);
                }
            }

            std::copy(
                reinterpret_cast<const byte_type*>(data),
                reinterpret_cast<const byte_type*>(data) + length,
                buffer.data() + packet.bytes);

            packet.bytes = nextPos;
        }

        template<typename IntType>
        void Write(IntType value)
        {
            static_assert(std::is_integral_v<IntType>);
            if constexpr (sizeof(IntType) == 1) {
                Write(&value, 1);
            } else {
                if (IsLittleEndian()) {
                    Write(&value, sizeof(IntType));
                } else {
                    IntType swapped = SwapIntBytes(value);
                    Write(&swapped, sizeof(IntType));
                }
            }
        }

        byte_type* GetBuffer()
        {
            return buffer.data();
        }

        size_t GetBufferSize() const
        {
            return buffer.size();
        }

        ogg_packet packet {};

    private:
        std::vector<byte_type> buffer;
        bool resizable { false };
    };

    struct
    {
        TranslatableString status;
        int32_t sampleRate {};

        double t0 {};
        double t1 {};
        unsigned numChannels {};
        wxFileNameWrapper fName;
        wxFile outFile;
        std::unique_ptr<Mixer> mixer;
        std::unique_ptr<Tags> metadata;

        // Encoder properties
        struct OpusState final
        {
            ~OpusState()
            {
                if (encoder != nullptr) {
                    opus_multistream_encoder_destroy(encoder);
                }
            }

            OpusMSEncoder* encoder {};

            int32_t frameSize {};
            int32_t sampleRateFactor {};
            uint16_t preskip {};
            uint8_t channelMapping {};
            uint8_t nbStreams {};
            uint8_t nbCoupled {};
            uint8_t streamMap[255] {};
        } opus;

        // Bitstream properties
        struct OggState final
        {
            OggState()
            // Audio always starts in the packet #3.
            // The first one is for opus header, the second one is for tags,
            // both are mandatory
                : audioStreamPacket(2)
            {
                // As per OGG docs - stream serialno should be a random
                // number. It is used to stitch the streams, this doesn't
                // matter much for Audacity
                std::mt19937 gen(std::time(nullptr));
                ogg_stream_init(&stream, gen());
            }

            void PacketIn(const OggPacket& packet)
            {
                ogg_stream_packetin(&stream,
                                    // C libraries are not always const-correct
                                    const_cast<ogg_packet*>(&packet.packet));
            }

            void WriteOut(wxFile& outputStream)
            {
                ogg_page page {};

                while (ogg_stream_pageout(&stream, &page)) {
                    WritePage(outputStream, page);
                }
            }

            void Flush(wxFile& outputStream)
            {
                ogg_page page {};

                while (ogg_stream_flush(&stream, &page)) {
                    WritePage(outputStream, page);
                }
            }

            ogg_stream_state stream;

            OggPacket audioStreamPacket;

        private:
            void WritePage(wxFile& outputStream, const ogg_page& page)
            {
                if (
                    outputStream.Write(page.header, page.header_len)
                    != page.header_len) {
                    FailExport(XO("Unable to write OGG page header"));
                }

                if (outputStream.Write(page.body, page.body_len) != page.body_len) {
                    FailExport(XO("Unable to write OGG page"));
                }
            }
        } ogg;

        std::vector<float> encodeBuffer;
    } context;

    void WriteOpusHeader();
    void WriteTags();

    int32_t GetBestFrameSize(int32_t samplesCount) const noexcept
    {
        static const int32_t multipliers[] = {
            25, 50, 100, 200, 400, 600,
        };

        const auto sampleRate = context.sampleRate;

        for (auto multiplier : multipliers) {
            const auto frameSize = multiplier * sampleRate / 10000;

            if (samplesCount <= frameSize) {
                return frameSize;
            }
        }

        return 60 * sampleRate / 1000;
    }

public:

    ~OpusExportProcessor();

    bool Initialize(AudacityProject& project, const Parameters& parameters, const wxFileNameWrapper& filename, double t0, double t1,
                    bool selectedOnly, double sampleRate, unsigned channels, MixerOptions::Downmix* mixerSpec, const Tags* tags) override;

    ExportResult Process(ExportProcessorDelegate& delegate) override;
};

class ExportOpus final : public ExportPlugin
{
public:

    ExportOpus();

    int GetFormatCount() const override;
    FormatInfo GetFormatInfo(int) const override;

    std::vector<std::string> GetMimeTypes(int) const override;

    std::unique_ptr<ExportOptionsEditor>
    CreateOptionsEditor(int, ExportOptionsEditor::Listener*) const override;

    std::unique_ptr<ExportProcessor> CreateProcessor(int format) const override;
};

ExportOpus::ExportOpus() = default;

int ExportOpus::GetFormatCount() const
{
    return 1;
}

FormatInfo ExportOpus::GetFormatInfo(int) const
{
    return {
        wxT("Opus"), XO("Opus Files"), { wxT("opus") }, 255, true
    };
}

std::vector<std::string> ExportOpus::GetMimeTypes(int) const
{
    return { "audio/opus" };
}

std::unique_ptr<ExportOptionsEditor>
ExportOpus::CreateOptionsEditor(int, ExportOptionsEditor::Listener* listener) const
{
    return std::make_unique<PlainExportOptionsEditor>(
        OPUSOptions,
        ExportOptionsEditor::SampleRateList { 8000, 12000, 16000, 24000, 48000 },
        listener);
}

std::unique_ptr<ExportProcessor> ExportOpus::CreateProcessor(int) const
{
    return std::make_unique<OpusExportProcessor>();
}

void OpusExportProcessor::WriteOpusHeader()
{
    const auto headerSize
        =// "OpusHead"
          8
          +// Version number (always 1)
          1
          +// Channels count
          1
          +// Preskip
          2
          +// Input sample rate
          4
          +// Output gain (always 0)
          2
          +// Channel mapping
          1
          + (context.opus.channelMapping == 0 ? 0
             : (
                 // Stream count
                 1
                 +// Two channel stream count
                 1
                 +// Channel mapping
                 context.numChannels));

    OggPacket headerPacket(0, headerSize, false);
    // Header must have beginning-of-stream marker
    headerPacket.MarkBOS();

    headerPacket.Write("OpusHead", 8);
    headerPacket.Write<uint8_t>(1);
    headerPacket.Write<uint8_t>(context.numChannels);
    headerPacket.Write(context.opus.preskip);
    // Should we put the project sample rate here?
    headerPacket.Write(context.sampleRate);
    // Opus docs recommend encoders to use 0 as a gain
    headerPacket.Write<uint16_t>(0);
    headerPacket.Write(context.opus.channelMapping);

    if (context.opus.channelMapping > 0) {
        headerPacket.Write(context.opus.nbStreams);
        headerPacket.Write(context.opus.nbCoupled);

        for (int i = 0; i < context.numChannels; ++i) {
            headerPacket.Write<uint8_t>(context.opus.streamMap[i]);
        }
    }

    // This is guaranteed by the way we calculate the header size
    assert(headerPacket.packet.bytes == headerSize);

    context.ogg.PacketIn(headerPacket);
    context.ogg.Flush(context.outFile);
}

void OpusExportProcessor::WriteTags()
{
    OggPacket commentsPacket { 1, true };

    commentsPacket.Write("OpusTags", 8);

    const std::string_view vendor { opus_get_version_string() };

    commentsPacket.Write<uint32_t>(vendor.size());
    commentsPacket.Write(vendor.data(), vendor.size());

    commentsPacket.Write<uint32_t>(context.metadata->Count());

    for (const auto& pair : context.metadata->GetRange()) {
        const auto key = pair.first == TAG_YEAR ? std::string("DATE")
                         : audacity::ToUTF8(pair.first);

        const auto value = audacity::ToUTF8(pair.second);

        commentsPacket.Write<uint32_t>(key.size() + value.size() + 1);
        commentsPacket.Write(key.data(), key.size());
        commentsPacket.Write("=", 1);
        commentsPacket.Write(value.data(), value.size());
    }

    context.ogg.PacketIn(commentsPacket);
    context.ogg.Flush(context.outFile);
}

OpusExportProcessor::~OpusExportProcessor()
{
}

bool OpusExportProcessor::Initialize(
    AudacityProject& project, const Parameters& parameters,
    const wxFileNameWrapper& fName, double t0, double t1, bool selectionOnly,
    double sampleRate, unsigned numChannels, MixerOptions::Downmix* mixerSpec,
    const Tags* metadata)
{
    context.sampleRate = int32_t(sampleRate);

    if (!IsValidSampleRate(context.sampleRate)) {
        throw ExportException(XO("Unsupported sample rate").Translation());
    }

    context.t0 = t0;
    context.t1 = t1;
    context.numChannels = numChannels;
    context.fName = fName;

    // Internally the Opus is always in 48k, find out the multiplier for
    // values, that expect 48k sample rate
    context.opus.sampleRateFactor = 48000 / context.sampleRate;

    const auto bitRate = ExportPluginHelpers::GetParameterValue<int>(
        parameters, OPUSOptionIDBitRate, OPUS_AUTO);
    const auto vbrMode = ExportPluginHelpers::GetParameterValue<int>(
        parameters, OPUSOptionIDVBRMode, VBRMode::VBR);
    const int complexity = ExportPluginHelpers::GetParameterValue<int>(
        parameters, OPUSOptionIDQuality, 10);
    const int frameMultiplier = ExportPluginHelpers::GetParameterValue<int>(
        parameters, OPUSOptionIDFrameDuration, 200);
    const int application = ExportPluginHelpers::GetParameterValue<int>(
        parameters, OPUSOptionIDApplication, OPUS_APPLICATION_AUDIO);
    const int cutoff = ExportPluginHelpers::GetParameterValue<int>(
        parameters, OPUSOptionIDCutoff, OPUS_AUTO);

    // Number of samples per frame per channel
    context.opus.frameSize = frameMultiplier * context.sampleRate / 10000;

    context.status = selectionOnly ? XO("Exporting selected audio as Opus")
                     : XO("Exporting the audio as Opus");

    // Create opus encoder
    int error;

    if (numChannels <= 2) {
        context.opus.channelMapping = 0;
        context.opus.nbStreams = 1;
        context.opus.nbCoupled = numChannels - 1;
        context.opus.streamMap[0] = 0;
        context.opus.streamMap[1] = 1;

        context.opus.encoder = opus_multistream_encoder_create(
            sampleRate, numChannels, context.opus.nbStreams,
            context.opus.nbCoupled, context.opus.streamMap, application, &error);
    } else {
        context.opus.channelMapping = numChannels <= 8 ? 1 : 255;

        int nbStreams {}, nbCoupled {};

        context.opus.encoder = opus_multistream_surround_encoder_create(
            sampleRate, numChannels, context.opus.channelMapping,
            &nbStreams, &nbCoupled,
            context.opus.streamMap, application, &error);

        // opus_multistream_surround_encoder_create is expected to fill
        // stream count with values in [0, 255]
        context.opus.nbStreams = uint8_t(nbStreams);
        context.opus.nbCoupled = uint8_t(nbCoupled);
    }

    if (error != OPUS_OK) {
        FailExport(XO("Unable to create Opus encoder"), error);
    }

    error = opus_multistream_encoder_ctl(
        context.opus.encoder, OPUS_SET_BITRATE(bitRate));

    if (error != OPUS_OK) {
        FailExport(XO("Unable to set bitrate"), error);
    }

    error = opus_multistream_encoder_ctl(
        context.opus.encoder, OPUS_SET_COMPLEXITY(complexity));

    if (error != OPUS_OK) {
        FailExport(XO("Unable to set complexity"), error);
    }

    error = opus_multistream_encoder_ctl(
        context.opus.encoder, OPUS_SET_BANDWIDTH(cutoff));

    if (error != OPUS_OK) {
        FailExport(XO("Unable to set bandwidth"), error);
    }

    error = opus_multistream_encoder_ctl(
        context.opus.encoder, OPUS_SET_VBR(vbrMode == VBRMode::CBR ? 0 : 1));

    if (error != OPUS_OK) {
        FailExport(XO("Unable to set VBR mode"), error);
    }

    if (vbrMode == VBRMode::CVBR) {
        error = opus_multistream_encoder_ctl(
            context.opus.encoder, OPUS_SET_VBR_CONSTRAINT(1));

        if (error != OPUS_OK) {
            FailExport(XO("Unable to set CVBR mode"), error);
        }
    }

    // Calculate the encoder latency. This value is needed in header
    // and to flush the encoder
    int lookahead {};
    error = opus_multistream_encoder_ctl(
        context.opus.encoder, OPUS_GET_LOOKAHEAD(&lookahead));

    if (error != OPUS_OK) {
        FailExport(XO("Unable to get lookahead"), error);
    }

    // Latency is always in 48k encoded samples
    const auto calculatedPreskip = lookahead * context.opus.sampleRateFactor;
    if (
        calculatedPreskip < 0
        || calculatedPreskip >= std::numeric_limits<uint16_t>::max()) {
        FailExport(XO("Failed to calculate correct preskip"), OPUS_BAD_ARG);
    }
    // It is safe to cast to uint16_t here
    context.opus.preskip = uint16_t(calculatedPreskip);

    // Resize the audio packet so it can contain all the raw data.
    // This is overkill, but should be enough to hold all the data from
    // the encode float
    context.ogg.audioStreamPacket.Resize(
        context.opus.frameSize * sizeof(float) * numChannels);

    // Try to open the file for writing
    if (
        !context.outFile.Create(fName.GetFullPath(), true)
        || !context.outFile.IsOpened()) {
        throw ExportException(_("Unable to open target file for writing"));
    }

    WriteOpusHeader();

    context.metadata = std::make_unique<Tags>(
        metadata == nullptr ? Tags::Get(project) : *metadata);

    WriteTags();

    context.mixer = ExportPluginHelpers::CreateMixer(
        project, selectionOnly, t0, t1, numChannels, context.opus.frameSize, true,
        sampleRate, floatSample, mixerSpec);

    return true;
}

ExportResult OpusExportProcessor::Process(ExportProcessorDelegate& delegate)
{
    delegate.SetStatusString(context.status);

    auto exportResult = ExportResult::Success;

    int64_t granulePos = 0;

    int32_t latencyLeft = context.opus.preskip;

    while (exportResult == ExportResult::Success)
    {
        auto samplesThisRun = context.mixer->Process();

        if (samplesThisRun == 0) {
            break;
        }

        auto mixedAudioBuffer
            =reinterpret_cast<const float*>(context.mixer->GetBuffer());

        // bestFrameSize <= context.opus.frameSize by design
        auto bestFrameSize = GetBestFrameSize(samplesThisRun);

        if (samplesThisRun < bestFrameSize) {
            // Opus expects that the full frame is passed to the encoder, fill missing data with zeroes
            context.encodeBuffer.resize(bestFrameSize * context.numChannels);

            std::copy(
                mixedAudioBuffer, mixedAudioBuffer + samplesThisRun * context.numChannels,
                context.encodeBuffer.begin());

            std::fill(
                context.encodeBuffer.begin() + samplesThisRun * context.numChannels,
                context.encodeBuffer.begin() + bestFrameSize * context.numChannels,
                0);

            mixedAudioBuffer = context.encodeBuffer.data();

            auto zeroesCount = bestFrameSize - int32_t(samplesThisRun);

            if (zeroesCount < latencyLeft) {
                samplesThisRun += zeroesCount;
            } else {
                samplesThisRun += latencyLeft;
            }

            // Reduce the latency by the number of zeroes pushed (potentially
            // removing the need to flush the encoder)
            latencyLeft = std::max(0, latencyLeft - zeroesCount);
        }

        auto result = opus_multistream_encode_float(
            context.opus.encoder, mixedAudioBuffer, bestFrameSize,
            context.ogg.audioStreamPacket.GetBuffer(),
            context.ogg.audioStreamPacket.GetBufferSize());

        if (result < 0) {
            FailExport(XO("Failed to encode input buffer"), result);
        }

        // granulePos is the index of the last real sample in the packet at 48k rate
        granulePos += samplesThisRun * context.opus.sampleRateFactor;

        context.ogg.audioStreamPacket.packet.bytes = result;
        context.ogg.audioStreamPacket.packet.granulepos = granulePos;

        if (latencyLeft == 0) {
            context.ogg.audioStreamPacket.MarkEOS();
        }

        context.ogg.PacketIn(context.ogg.audioStreamPacket);
        context.ogg.WriteOut(context.outFile);

        context.ogg.audioStreamPacket.packet.packetno++;

        exportResult = ExportPluginHelpers::UpdateProgress(
            delegate, *context.mixer, context.t0, context.t1);
    }

    // Flush the encoder

    while (latencyLeft > 0)
    {
        auto frameSize = GetBestFrameSize(latencyLeft);

        context.encodeBuffer.resize(frameSize * context.numChannels);

        std::fill(
            context.encodeBuffer.begin(),
            context.encodeBuffer.begin() + frameSize * context.numChannels, 0);

        auto samplesOut = std::min(latencyLeft, frameSize);

        auto result = opus_multistream_encode_float(
            context.opus.encoder, context.encodeBuffer.data(),
            frameSize, context.ogg.audioStreamPacket.GetBuffer(),
            context.ogg.audioStreamPacket.GetBufferSize());

        if (result < 0) {
            FailExport(XO("Failed to encode input buffer"), result);
        }

        granulePos += samplesOut * context.opus.sampleRateFactor;

        context.ogg.audioStreamPacket.packet.bytes = result;
        context.ogg.audioStreamPacket.packet.granulepos = granulePos;

        if (latencyLeft == samplesOut) {
            context.ogg.audioStreamPacket.MarkEOS();
        }

        context.ogg.PacketIn(context.ogg.audioStreamPacket);
        context.ogg.WriteOut(context.outFile);

        context.ogg.audioStreamPacket.packet.packetno++;

        latencyLeft -= samplesOut;
    }

    context.ogg.Flush(context.outFile);

    if (!context.outFile.Close()) {
        return ExportResult::Error;
    }

    return exportResult;
}

static ExportPluginRegistry::RegisteredPlugin sRegisteredPlugin{ "Opus",
                                                                 []{ return std::make_unique< ExportOpus >(); }
};
