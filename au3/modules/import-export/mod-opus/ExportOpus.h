/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "au3-import-export/Export.h"
#include "au3-import-export/ExportPluginHelpers.h"
#include "au3-import-export/ExportPluginRegistry.h"
#include "au3-import-export/ExportOptionsEditor.h"
#include "au3-import-export/PlainExportOptionsEditor.h"

#include "au3-string-utils/CodeConversions.h"

#include "au3-mixer/Mix.h"
#include "au3-utility/MemoryX.h"

#include "au3-tags/Tags.h"
#include "au3-track/Track.h"

#include <random>
#include <string_view>

#include <ogg/ogg.h>
#include <opus/opus.h>
#include <opus/opus_multistream.h>

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

    int32_t GetBestFrameSize(int32_t samplesCount) const noexcept;

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
