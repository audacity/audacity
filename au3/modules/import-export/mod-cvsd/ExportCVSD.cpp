#include "ExportCVSD.h"

void CVSDEncode(constSamplePtr temp, unsigned int channels)
{

};

void CVSDDecode(std::unique_ptr<wxFFile> openedFile)
{

};

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
    // set the context
    context.status = TranslatableString("import-export", "Exporting the selected audio as CVSD");
    context.mfile = std::make_unique<FileIO>(filename, FileIO::Output);
    context.t0 = t0;
    context.t1 = t1;
    context.channels = 1;
    context.max_block_len = 44100*5;

    if (!context.mfile->IsOpened()) {
        return false;
    }

    // get the config
    const size_t maxBlockLen = context.max_block_len;
    unsigned cvsdNumOfChannels = context.channels;

    // using floatSample because CVSD, math is easier with floats (-1.0 to 1.0)
    context.mMixer = ExportPluginHelpers::CreateMixer(
        project, selectedOnly, t0, t1,
        cvsdNumOfChannels,
        maxBlockLen, true, sampleRate,
        floatSample, mixerSpec);

    // resetting config to default
    config.mAccumulator = 0.0f;
    config.step_size = 10.0f;

    return (context.mMixer != nullptr);
};

ExportResult ExportCVSDProcessor::Process(ExportProcessorDelegate& delegate)
{
    delegate.SetStatusString(context.status);
    auto exportResult = ExportResult::Success;
    {
        int eos = 0;
        while (exportResult == ExportResult::Success && !eos)
        {
            // get the samples
            auto samplesThisRun = context.mMixer->Process();
            if (samplesThisRun == 0 )
            {
                context.status = TranslatableString("import-export", "Exporting is done");
                delegate.SetStatusString(context.status);
                break;
            } else {
                for (size_t i = 0; i < context.channels; i++) {
                     constSamplePtr temp = context.mMixer->GetBuffer(i);
                    CVSDEncode(temp, context.channels);
                }
            }
        }
    }
};

std::unique_ptr<ExportOptionsEditor> ExportCVSD::CreateOptionsEditor(int formatIndex, ExportOptionsEditor::Listener* listener) const
{
    return nullptr;
}
