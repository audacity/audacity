/**********************************************************************

   SPDX-License-Identifier: GPL-2.0-or-later

   Audacity: A Digital Audio Editor

   ExportWavPack.cpp

   Subhradeep Chakraborty

   Based on ExportOGG.cpp, ExportMP2.cpp by:
   Joshua Haberman
   Markus Meyer

**********************************************************************/

#include "Export.h"
#include "wxFileNameWrapper.h"
#include "Mix.h"

#include <wavpack/wavpack.h>

#include <rapidjson/document.h>

#include "Track.h"
#include "Tags.h"

#include "ExportPluginHelpers.h"
#include "ExportOptionsEditor.h"
#include "ExportPluginRegistry.h"

namespace {
enum : int {
    OptionIDQuality = 0,
    OptionIDBitDepth,
    OptionIDHybridMode,
    OptionIDCreateCorrection,
    OptionIDBitRate
};

const TranslatableStrings ExportQualityNames{
    XO("Low Quality (Fast)"),
    XO("Normal Quality"),
    XO("High Quality (Slow)"),
    XO("Very High Quality (Slowest)"),
};

const TranslatableStrings ExportBitDepthNames {
    XO("16 bit"),
    XO("24 bit"),
    XO("32 bit float"),
};

/*
Copied from ExportMP2.cpp by
   Joshua Haberman
   Markus Meyer
*/

// i18n-hint bps abbreviates "bits per sample"
inline TranslatableString n_bps(int n) { return XO("%.1f bps").Format(n / 10.0); }

const TranslatableStrings BitRateNames {
    n_bps(22),
    n_bps(25),
    n_bps(30),
    n_bps(35),
    n_bps(40),
    n_bps(45),
    n_bps(50),
    n_bps(60),
    n_bps(70),
    n_bps(80),
};

const std::initializer_list<ExportOption> ExportWavPackOptions {
    {
        OptionIDQuality, XO("Quality"),
        1,
        ExportOption::TypeEnum,
        { 0, 1, 2, 3 },
        ExportQualityNames
    },
    {
        OptionIDBitDepth, XO("Bit Depth"),
        16,
        ExportOption::TypeEnum,
        { 16, 24, 32 },
        ExportBitDepthNames
    },
    {
        OptionIDHybridMode, XO("Hybrid Mode"),
        false
    },
    {
        OptionIDCreateCorrection, XO("Create Correction(.wvc) File"),
        false,
        ExportOption::ReadOnly
    },
    {
        OptionIDBitRate, XO("Bit Rate"),
        40,
        ExportOption::TypeEnum,
        { 22, 25, 30, 35, 40, 45, 50, 60, 70, 80 },
        BitRateNames
    }
};

class ExportOptionsWavPackEditor final : public ExportOptionsEditor
{
    Listener* mListener{ nullptr };
    std::vector<ExportOption> mOptions = ExportWavPackOptions;
    std::unordered_map<ExportOptionID, ExportValue> mValues;
public:

    ExportOptionsWavPackEditor(Listener* listener)
        : mListener(listener)
    {
        for (const auto& option : mOptions) {
            mValues[option.id] = option.defaultValue;
        }
    }

    int GetOptionsCount() const override
    {
        return static_cast<int>(mOptions.size());
    }

    bool GetOption(int index, ExportOption& option) const override
    {
        if (index >= 0 && index < mOptions.size()) {
            option = mOptions[index];
            return true;
        }
        return false;
    }

    bool GetValue(ExportOptionID id, ExportValue& value) const override
    {
        const auto it = mValues.find(id);
        if (it != mValues.end()) {
            value = it->second;
            return true;
        }
        return false;
    }

    bool SetValue(ExportOptionID id, const ExportValue& value) override
    {
        auto it = mValues.find(id);
        if (it == mValues.end() || value.index() != it->second.index()) {
            return false;
        }

        it->second = value;
        if (id == OptionIDHybridMode) {
            OnHybridModeChange(*std::get_if<bool>(&value));

            if (mListener) {
                mListener->OnExportOptionChangeBegin();
                mListener->OnExportOptionChange(mOptions[OptionIDCreateCorrection]);
                mListener->OnExportOptionChange(mOptions[OptionIDBitRate]);
                mListener->OnExportOptionChangeEnd();
            }
        }
        return true;
    }

    SampleRateList GetSampleRateList() const override
    {
        return {};
    }

    void Load(const audacity::BasicSettings& config) override
    {
        auto quality = std::get_if<int>(&mValues[OptionIDQuality]);
        auto bitDepth = std::get_if<int>(&mValues[OptionIDBitDepth]);
        auto hybridMode = std::get_if<bool>(&mValues[OptionIDHybridMode]);
        auto createCorrection = std::get_if<bool>(&mValues[OptionIDCreateCorrection]);
        auto bitRate = std::get_if<int>(&mValues[OptionIDBitRate]);

        config.Read(L"/FileFormats/WavPackEncodeQuality", quality);
        config.Read(L"/FileFormats/WavPackBitDepth", bitDepth);
        config.Read(L"/FileFormats/WavPackHybridMode", hybridMode);
        config.Read(L"/FileFormats/WavPackCreateCorrectionFile", createCorrection);
        config.Read(L"/FileFormats/WavPackBitrate", bitRate);

        OnHybridModeChange(*hybridMode);
    }

    void Store(audacity::BasicSettings& config) const override
    {
        auto it = mValues.find(OptionIDQuality);
        if (it != mValues.end()) {
            config.Write(L"/FileFormats/WavPackEncodeQuality", *std::get_if<int>(&it->second));
        }

        it = mValues.find(OptionIDBitDepth);
        if (it != mValues.end()) {
            config.Write(L"/FileFormats/WavPackBitDepth", *std::get_if<int>(&it->second));
        }

        it = mValues.find(OptionIDHybridMode);
        if (it != mValues.end()) {
            config.Write(L"/FileFormats/WavPackHybridMode", *std::get_if<bool>(&it->second));
        }

        it = mValues.find(OptionIDCreateCorrection);
        if (it != mValues.end()) {
            config.Write(L"/FileFormats/WavPackCreateCorrectionFile", *std::get_if<bool>(&it->second));
        }

        it = mValues.find(OptionIDBitRate);
        if (it != mValues.end()) {
            config.Write(L"/FileFormats/WavPackBitrate", *std::get_if<int>(&it->second));
        }
    }

private:
    void OnHybridModeChange(bool hybridMode)
    {
        if (hybridMode) {
            mOptions[OptionIDCreateCorrection].flags &= ~ExportOption::Flags::ReadOnly;
            mOptions[OptionIDBitRate].flags &= ~ExportOption::Flags::ReadOnly;
        } else {
            mOptions[OptionIDCreateCorrection].flags |= ExportOption::Flags::ReadOnly;
            mOptions[OptionIDBitRate].flags |= ExportOption::Flags::ReadOnly;
        }
    }
};
}

struct WriteId final
{
    uint32_t bytesWritten {};
    uint32_t firstBlockSize {};
    std::unique_ptr<wxFile> file;
};

class WavPackExportProcessor final : public ExportProcessor
{
    // Samples to write per run
    static constexpr size_t SAMPLES_PER_RUN = 8192u;

    struct
    {
        TranslatableString status;
        double t0;
        double t1;
        unsigned numChannels;
        wxFileNameWrapper fName;
        sampleFormat format;
        WriteId outWvFile, outWvcFile;
        WavpackContext* wpc{};
        std::unique_ptr<Mixer> mixer;
        std::unique_ptr<Tags> metadata;
    } context;
public:

    ~WavPackExportProcessor();

    bool Initialize(AudacityProject& project, const Parameters& parameters, const wxFileNameWrapper& filename, double t0, double t1,
                    bool selectedOnly, double sampleRate, unsigned channels, MixerOptions::Downmix* mixerSpec, const Tags* tags) override;

    ExportResult Process(ExportProcessorDelegate& delegate) override;

private:
    static int WriteBlock(void* id, void* data, int32_t length);
};

class ExportWavPack final : public ExportPlugin
{
public:

    ExportWavPack();

    int GetFormatCount() const override;
    FormatInfo GetFormatInfo(int) const override;

    std::vector<std::string> GetMimeTypes(int) const override;

    bool ParseConfig(int formatIndex, const rapidjson::Value& document, ExportProcessor::Parameters& parameters) const override;

    std::unique_ptr<ExportOptionsEditor>
    CreateOptionsEditor(int, ExportOptionsEditor::Listener*) const override;

    std::unique_ptr<ExportProcessor> CreateProcessor(int format) const override;
};

ExportWavPack::ExportWavPack() = default;

int ExportWavPack::GetFormatCount() const
{
    return 1;
}

FormatInfo ExportWavPack::GetFormatInfo(int) const
{
    return {
        wxT("WavPack"), XO("WavPack Files"), { wxT("wv") }, 255, true
    };
}

std::vector<std::string> ExportWavPack::GetMimeTypes(int) const
{
    return { "audio/x-wavpack" };
}

bool ExportWavPack::ParseConfig(int formatIndex, const rapidjson::Value& config, ExportProcessor::Parameters& parameters) const
{
    if (!config.IsObject()
        || !config.HasMember("quality") || !config["quality"].IsNumber()
        || !config.HasMember("bit_rate") || !config["bit_rate"].IsNumber()
        || !config.HasMember("bit_depth") || !config["bit_depth"].IsNumber()
        || !config.HasMember("hybrid_mode") || !config["hybrid_mode"].IsBool()) {
        return false;
    }

    const auto quality = ExportValue(config["quality"].GetInt());
    const auto bitRate = ExportValue(config["bit_rate"].GetInt());
    const auto bitDepth = ExportValue(config["bit_depth"].GetInt());
    const auto hybridMode = ExportValue(config["hybrid_mode"].GetBool());

    for (const auto& option : ExportWavPackOptions) {
        if ((option.id == OptionIDQuality
             && std::find(option.values.begin(),
                          option.values.end(),
                          quality) == option.values.end())
            ||
            (option.id == OptionIDBitRate
             && std::find(option.values.begin(),
                          option.values.end(),
                          bitRate) == option.values.end())
            ||
            (option.id == OptionIDBitDepth
             && std::find(option.values.begin(),
                          option.values.end(),
                          bitDepth) == option.values.end())) {
            return false;
        }
    }
    ExportProcessor::Parameters result {
        { OptionIDQuality, quality },
        { OptionIDBitRate, bitRate },
        { OptionIDBitDepth, bitDepth },
        { OptionIDHybridMode, hybridMode },
        { OptionIDCreateCorrection, false }
    };
    std::swap(parameters, result);
    return true;
}

std::unique_ptr<ExportOptionsEditor>
ExportWavPack::CreateOptionsEditor(int, ExportOptionsEditor::Listener* listener) const
{
    return std::make_unique<ExportOptionsWavPackEditor>(listener);
}

std::unique_ptr<ExportProcessor> ExportWavPack::CreateProcessor(int) const
{
    return std::make_unique<WavPackExportProcessor>();
}

WavPackExportProcessor::~WavPackExportProcessor()
{
    if (context.wpc) {
        WavpackCloseFile(context.wpc);
    }
}

bool WavPackExportProcessor::Initialize(AudacityProject& project,
                                        const Parameters& parameters,
                                        const wxFileNameWrapper& fName,
                                        double t0, double t1, bool selectionOnly,
                                        double sampleRate, unsigned numChannels,
                                        MixerOptions::Downmix* mixerSpec,
                                        const Tags* metadata)
{
    context.t0 = t0;
    context.t1 = t1;
    context.numChannels = numChannels;
    context.fName = fName;

    WavpackConfig config = {};
    auto& outWvFile = context.outWvFile;
    auto& outWvcFile = context.outWvcFile;
    outWvFile.file = std::make_unique< wxFile >();

    if (!outWvFile.file->Create(fName.GetFullPath(), true) || !outWvFile.file->IsOpened()) {
        throw ExportException(_("Unable to open target file for writing"));
    }

    const auto quality = ExportPluginHelpers::GetParameterValue<int>(
        parameters,
        OptionIDQuality,
        1);
    const auto hybridMode = ExportPluginHelpers::GetParameterValue<bool>(
        parameters,
        OptionIDHybridMode,
        false);
    const auto createCorrectionFile = ExportPluginHelpers::GetParameterValue<bool>(
        parameters,
        OptionIDCreateCorrection,
        false);
    const auto bitRate = ExportPluginHelpers::GetParameterValue<int>(
        parameters,
        OptionIDBitRate,
        40);
    const auto bitDepth = ExportPluginHelpers::GetParameterValue<int>(
        parameters,
        OptionIDBitDepth,
        16);

    context.format = int16Sample;
    if (bitDepth == 24) {
        context.format = int24Sample;
    } else if (bitDepth == 32) {
        context.format = floatSample;
    }

    config.num_channels = numChannels;
    config.sample_rate = sampleRate;
    config.bits_per_sample = bitDepth;
    config.bytes_per_sample = bitDepth / 8;
    config.float_norm_exp = context.format == floatSample ? 127 : 0;

    if (config.num_channels <= 2) {
        config.channel_mask = 0x5 - config.num_channels;
    } else if (config.num_channels <= 18) {
        config.channel_mask = (1U << config.num_channels) - 1;
    } else {
        config.channel_mask = 0x3FFFF;
    }

    if (quality == 0) {
        config.flags |= CONFIG_FAST_FLAG;
    } else if (quality == 2) {
        config.flags |= CONFIG_HIGH_FLAG;
    } else if (quality == 3) {
        config.flags |= CONFIG_HIGH_FLAG | CONFIG_VERY_HIGH_FLAG;
    }

    if (hybridMode) {
        config.flags |= CONFIG_HYBRID_FLAG;
        config.bitrate = bitRate / 10.0;

        if (createCorrectionFile) {
            config.flags |= CONFIG_CREATE_WVC;

            outWvcFile.file = std::make_unique< wxFile >();
            if (!outWvcFile.file->Create(fName.GetFullPath().Append("c"), true)) {
                throw ExportException(_("Unable to create target file for writing"));
            }
        }
    }

    // If we're not creating a correction file now, any one that currently exists with this name
    // will become obsolete now, so delete it if it happens to exist (although it usually won't)

    if (!hybridMode || !createCorrectionFile) {
        wxRemoveFile(fName.GetFullPath().Append("c"));
    }

    context.wpc = WavpackOpenFileOutput(WriteBlock, &outWvFile, createCorrectionFile ? &outWvcFile : nullptr);
    if (!WavpackSetConfiguration64(context.wpc, &config, -1, nullptr) || !WavpackPackInit(context.wpc)) {
        throw ExportErrorException(WavpackGetErrorMessage(context.wpc));
    }

    context.status = selectionOnly
                     ? XO("Exporting selected audio as WavPack")
                     : XO("Exporting the audio as WavPack");

    context.metadata = std::make_unique<Tags>(
        metadata == nullptr
        ? Tags::Get(project)
        : *metadata
        );

    context.mixer = ExportPluginHelpers::CreateMixer(
        project, selectionOnly, t0, t1, numChannels, SAMPLES_PER_RUN, true,
        sampleRate, context.format, mixerSpec);

    return true;
}

ExportResult WavPackExportProcessor::Process(ExportProcessorDelegate& delegate)
{
    delegate.SetStatusString(context.status);

    const size_t bufferSize = SAMPLES_PER_RUN * context.numChannels;

    ArrayOf<int32_t> wavpackBuffer{ bufferSize };

    auto exportResult = ExportResult::Success;
    {
        while (exportResult == ExportResult::Success) {
            auto samplesThisRun = context.mixer->Process();

            if (samplesThisRun == 0) {
                break;
            }

            if (context.format == int16Sample) {
                const int16_t* mixed = reinterpret_cast<const int16_t*>(context.mixer->GetBuffer());
                for (decltype(samplesThisRun) j = 0; j < samplesThisRun; j++) {
                    for (size_t i = 0; i < context.numChannels; i++) {
                        wavpackBuffer[j * context.numChannels + i] = (static_cast<int32_t>(*mixed++) * 65536) >> 16;
                    }
                }
            } else {
                const int* mixed = reinterpret_cast<const int*>(context.mixer->GetBuffer());
                for (decltype(samplesThisRun) j = 0; j < samplesThisRun; j++) {
                    for (size_t i = 0; i < context.numChannels; i++) {
                        wavpackBuffer[j * context.numChannels + i] = *mixed++;
                    }
                }
            }

            if (!WavpackPackSamples(context.wpc, wavpackBuffer.get(), samplesThisRun)) {
                throw ExportErrorException(WavpackGetErrorMessage(context.wpc));
            }
            exportResult = ExportPluginHelpers::UpdateProgress(
                delegate, *context.mixer, context.t0, context.t1);
        }
    }

    if (!WavpackFlushSamples(context.wpc)) {
        throw ExportErrorException(WavpackGetErrorMessage(context.wpc));
    } else {
        wxString n;
        for (const auto& pair : context.metadata->GetRange()) {
            n = pair.first;
            const auto& v = pair.second;

            WavpackAppendTagItem(context.wpc,
                                 n.mb_str(wxConvUTF8),
                                 v.mb_str(wxConvUTF8),
                                 static_cast<int>(strlen(v.mb_str(wxConvUTF8))));
        }

        if (!WavpackWriteTag(context.wpc)) {
            throw ExportErrorException(WavpackGetErrorMessage(context.wpc));
        }
    }

    if (!context.outWvFile.file.get()->Close()
        || (context.outWvcFile.file && context.outWvcFile.file.get() && !context.outWvcFile.file.get()->Close())) {
        return ExportResult::Error;
    }

    // wxFile::Create opens the file with only write access
    // So, need to open the file again with both read and write access
    if (!context.outWvFile.file->Open(context.fName.GetFullPath(), wxFile::read_write)) {
        throw ExportErrorException("Unable to update the actual length of the file");
    }

    ArrayOf<int32_t> firstBlockBuffer { context.outWvFile.firstBlockSize };
    context.outWvFile.file->Read(firstBlockBuffer.get(), context.outWvFile.firstBlockSize);

    // Update the first block written with the actual number of samples written
    WavpackUpdateNumSamples(context.wpc, firstBlockBuffer.get());
    context.outWvFile.file->Seek(0);
    context.outWvFile.file->Write(firstBlockBuffer.get(), context.outWvFile.firstBlockSize);

    if (!context.outWvFile.file.get()->Close()) {
        return ExportResult::Error;
    }
    return exportResult;
}

// Based on the implementation of write_block in dbry/WavPack
// src: https://github.com/dbry/WavPack/blob/master/cli/wavpack.c
int WavPackExportProcessor::WriteBlock(void* id, void* data, int32_t length)
{
    if (id == nullptr || data == nullptr || length == 0) {
        return true; // This is considered to be success in wavpack.c reference code
    }
    WriteId* outId = static_cast<WriteId*>(id);

    if (!outId->file) {
        // This does not match the wavpack.c but in our case if file is nullptr -
        // the stream error has occured
        return false;
    }

    //  if (!outId->file->Write(data, length).IsOk()) {
    if (outId->file->Write(data, length) != length) {
        outId->file.reset();
        return false;
    }

    outId->bytesWritten += length;

    if (outId->firstBlockSize == 0) {
        outId->firstBlockSize = length;
    }

    return true;
}

static ExportPluginRegistry::RegisteredPlugin sRegisteredPlugin{ "WavPack",
                                                                 []{ return std::make_unique< ExportWavPack >(); }
};
