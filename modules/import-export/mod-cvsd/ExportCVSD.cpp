#import "ExportPlugin.h"
#include "ExportPluginHelpers.h"
#include "FileIO.h"
#include "Mix.h"
#include "CVSD.cpp"


class ExportCVSD final : public ExportPlugin
{
public:
    ExportCVSD() = default;

    ~ExportCVSD();

    int GetFormatCount() const override;
    FormatInfo GetFormatInfo(int) const override;

    std::unique_ptr<ExportOptionsEditor>
    CreateOptionsEditor(int, ExportOptionsEditor::Listener*) const override;

    std::unique_ptr<ExportProcessor> CreateProcessor(int format) const override;
};

class ExportCVSDProcessor final : public ExportProcessor
{
private:
    std::unique_ptr<FileIO> mfile;
    std::unique_ptr<Mixer> mMixer;
public:
    ExportCVSDProcessor();

    ~ExportCVSDProcessor();

    bool Initialize(AudacityProject& project,
     const Parameters& parameters,
     const wxFileNameWrapper& filename,
     double t0, double t1, bool selectedOnly,
     double sampleRate, unsigned channels,
     MixerOptions::Downmix* mixerSpec,
     const Tags* tags) override
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
    }

    ExportResult Process(ExportProcessorDelegate& delegate) override
    {
        return ExportResult::Success;
    };
};

