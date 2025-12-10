/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "au3-import-export/Export.h"

#include "au3-mixer/Mix.h"
#include "au3-preferences/Prefs.h"

#include "au3-tags/Tags.h"
#include "au3-track/Track.h"

#include "au3-import-export/ExportPluginHelpers.h"
#include "au3-import-export/ExportPluginRegistry.h"
#include "au3-import-export/PlainExportOptionsEditor.h"

#include "FLAC++/encoder.h"

enum : int {
    FlacOptionIDBitDepth = 0,
    FlacOptionIDLevel
};

struct FLAC__StreamMetadataDeleter {
    void operator ()(FLAC__StreamMetadata* p) const
    {
        if (p) {
            ::FLAC__metadata_object_delete(p);
        }
    }
};
using FLAC__StreamMetadataHandle = std::unique_ptr<
    FLAC__StreamMetadata, FLAC__StreamMetadataDeleter
    >;

class FLACExportProcessor final : public ExportProcessor
{
    struct
    {
        TranslatableString status;
        double t0;
        double t1;
        unsigned numChannels;
        wxFileNameWrapper fName;
        sampleFormat format;
        FLAC::Encoder::File encoder;
        wxFFile f;
        std::unique_ptr<Mixer> mixer;
    } context;

public:

    bool Initialize(AudacityProject& project, const Parameters& parameters, const wxFileNameWrapper& filename, double t0, double t1,
                    bool selectedOnly, double sampleFormat, unsigned channels, MixerOptions::Downmix* mixerSpec, const Tags* tags) override;

    ExportResult Process(ExportProcessorDelegate& delegate) override;

private:

    FLAC__StreamMetadataHandle MakeMetadata(AudacityProject* project, const Tags* tags) const;
};

class ExportFLAC final : public ExportPlugin
{
public:

    ExportFLAC();

    int GetFormatCount() const override;
    FormatInfo GetFormatInfo(int) const override;

    // bool ParseConfig(int, const rapidjson::Value& config, ExportProcessor::Parameters& parameters) const override;

    std::vector<std::string> GetMimeTypes(int) const override;

    // Required

    std::unique_ptr<ExportOptionsEditor>
    CreateOptionsEditor(int, ExportOptionsEditor::Listener* listener) const override;

    std::unique_ptr<ExportProcessor> CreateProcessor(int format) const override;
};
