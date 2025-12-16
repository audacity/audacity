/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "au3-import-export/Export.h"

#include "au3-import-export/ExportPluginHelpers.h"
#include "au3-import-export/ExportPluginRegistry.h"

#include "au3-files/FileIO.h"
#include "au3-mixer/Mix.h"

#include "au3-tags/Tags.h"
#include "au3-track/Track.h"

#include "au3-import-export/PlainExportOptionsEditor.h"

#include <vorbis/vorbisenc.h>

#define SAMPLES_PER_RUN 8192u

class ExportOptionOGGEditor final : public ExportOptionsEditor
{
public:

    ExportOptionOGGEditor();

    std::string GetName() const override;
    int GetOptionsCount() const override;
    bool GetOption(int, ExportOption& option) const override;
    bool GetValue(ExportOptionID, ExportValue& value) const override;
    bool SetValue(ExportOptionID, const ExportValue& value) override;
    SampleRateList GetSampleRateList() const override;
    void Load(const audacity::BasicSettings& config) override;
    void Store(audacity::BasicSettings& config) const override;

private:
    int mQualityUnscaled;
};

class OGGExportProcessor final : public ExportProcessor
{
    struct
    {
        TranslatableString status;
        double t0;
        double t1;
        unsigned numChannels;
        std::unique_ptr<Mixer> mixer;
        std::unique_ptr<FileIO> outFile;
        wxFileNameWrapper fName;

        // All the Ogg and Vorbis encoding data
        ogg_stream_state stream;
        ogg_page page;
        ogg_packet packet;

        vorbis_info info;
        vorbis_comment comment;
        vorbis_dsp_state dsp;
        vorbis_block block;
        bool stream_ok{ false };
        bool analysis_state_ok{ false };
    } context;
public:
    ~OGGExportProcessor() override;

    bool Initialize(AudacityProject& project, const Parameters& parameters, const wxFileNameWrapper& filename, double t0, double t1,
                    bool selectedOnly, double sampleRate, unsigned channels, MixerOptions::Downmix* mixerSpec, const Tags* tags) override;

    ExportResult Process(ExportProcessorDelegate& delegate) override;

private:
    static void FillComment(AudacityProject* project, vorbis_comment* comment, const Tags* metadata);
};

class ExportOGG final : public ExportPlugin
{
public:

    ExportOGG();

    int GetFormatCount() const override;
    FormatInfo GetFormatInfo(int) const override;

    std::unique_ptr<ExportOptionsEditor>
    CreateOptionsEditor(int, ExportOptionsEditor::Listener*) const override;

    std::unique_ptr<ExportProcessor> CreateProcessor(int format) const override;
};
