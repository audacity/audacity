#include "au3-import-export/ExportPlugin.h"
#include "au3-import-export/ExportPluginHelpers.h"
#include "CVSD.h"
#include "au3-files/FileIO.h"
#include "au3-mixer/Mix.h"

#ifndef AUDACITY_EXPORTCVSD_H
#define AUDACITY_EXPORTCVSD_H

class ExportOptionCVSDEditor final : public ExportOptionsEditor
{
public:

    ExportOptionCVSDEditor() = default;

    std::string GetName() const override;
    int GetOptionsCount() const override;
    bool GetOption(int, ExportOption& option) const override;
    bool GetValue(ExportOptionID, ExportValue& value) const override;
    bool SetValue(ExportOptionID, const ExportValue& value) override;

    SampleRateList GetSampleRateList() const override;

    void Load(const audacity::BasicSettings& config) override;
    void Store(audacity::BasicSettings& config) const override;

private:
    int mQualityUnscaled = 0;
};

class ExportCVSD final : public ExportPlugin
{
public:
    ExportCVSD() = default;

    ~ExportCVSD() = default;

    int GetFormatCount() const override;
    FormatInfo GetFormatInfo(int) const override;

    std::unique_ptr<ExportOptionsEditor>
    CreateOptionsEditor(int, ExportOptionsEditor::Listener*) const override;

    std::unique_ptr<ExportProcessor> CreateProcessor(int format) const override;
};

class ExportCVSDProcessor final : public ExportProcessor
{
private:
    CVSD_CONFIG config;
    struct {
        std::unique_ptr<FileIO> mfile;
        std::unique_ptr<Mixer> mMixer;
        TranslatableString status;
        double t0;
        double t1;
        size_t max_block_len;
        unsigned channels;
    } context;
public:
    ExportCVSDProcessor() = default;

    ~ExportCVSDProcessor() = default;

    bool Initialize(AudacityProject& project,
     const Parameters& parameters,
     const wxFileNameWrapper& filename,
     double t0, double t1, bool selectedOnly,
     double sampleRate, unsigned channels,
     MixerOptions::Downmix* mixerSpec,
     const Tags* tags) override;

    ExportResult Process(ExportProcessorDelegate& delegate) override;

    std::unique_ptr<ExportOptionsEditor> CreateOptionsEditor(int formatIndex, ExportOptionsEditor::Listener* listener) const;
};

#endif //AUDACITY_EXPORTCVSD_H
