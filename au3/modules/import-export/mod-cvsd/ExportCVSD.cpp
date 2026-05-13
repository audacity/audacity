#include "ExportCVSD.h"

int ExportCVSD::GetFormatCount() const
{
    return 1;
};

FormatInfo ExportCVSD::GetFormatInfo(int) const
{
    return {
        wxT("CVSD"), XO("CVSD Files"), { wxT("cvsd") }, 1, true
    };
};

std::unique_ptr<ExportProcessor> ExportCVSD::CreateProcessor(int format) const
{
    return std::make_unique<ExportCVSDProcessor>();
}

bool ExportCVSDProcessor::Initialize(AudacityProject& project,
     const Parameters& parameters,
     const wxFileNameWrapper& filename,
     double t0, double t1, bool selectedOnly,
     double sampleRate, unsigned channels,
     MixerOptions::Downmix* mixerSpec,
     const Tags* tags)
{
    mfile = std::make_unique<FileIO>(filename, FileIO::Output);
    if (!mfile->IsOpened()) {
        return false;
    }

    const size_t maxBlockLen = config.max_block_len;
    unsigned cvsdNumOfChannels = config.channels;

    // using floatSample because CVSD, math is easier with floats (-1.0 to 1.0)
    mMixer = ExportPluginHelpers::CreateMixer(
        project, selectedOnly, t0, t1,
        cvsdNumOfChannels,
        maxBlockLen, true, sampleRate,
        floatSample, mixerSpec);

    // resetting config to default
    config.mAccumulator = 0.0f;
    config.step_size = 10.0f;

    return (mMixer != nullptr);
};

ExportResult ExportCVSDProcessor::Process(ExportProcessorDelegate& delegate)
{
    return ExportResult::Success;
};

std::unique_ptr<ExportOptionsEditor> ExportCVSD::CreateOptionsEditor(int formatIndex, ExportOptionsEditor::Listener* listener) const
{
    return nullptr;
}
