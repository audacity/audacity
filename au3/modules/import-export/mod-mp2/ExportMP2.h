/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "au3-import-export/Export.h"
#include "au3-import-export/ExportPluginHelpers.h"
#include "au3-import-export/ExportPluginRegistry.h"
#include "au3-import-export/ExportOptionsEditor.h"
#include "au3-import-export/PlainExportOptionsEditor.h"

#include "au3-files/FileIO.h"
#include "au3-mixer/Mix.h"
#include "au3-tags/Tags.h"
#include "au3-track/Track.h"

#define LIBTWOLAME_STATIC
#include "twolame.h"

namespace {
// i18n-hint kbps abbreviates "thousands of bits per second"
inline TranslatableString n_kbps(int n) { return XO("%d kbps").Format(n); }

const TranslatableStrings BitRateMPEG1Names {
    n_kbps(32),
    n_kbps(48),
    n_kbps(56),
    n_kbps(64),
    n_kbps(80),
    n_kbps(96),
    n_kbps(112),
    n_kbps(128),
    n_kbps(160),
    n_kbps(192),//default
    n_kbps(224),
    n_kbps(256),
    n_kbps(320),
    n_kbps(384),
};

const TranslatableStrings BitRateMPEG2Names {
    n_kbps(8),
    n_kbps(16),
    n_kbps(24),
    n_kbps(32),
    n_kbps(40),
    n_kbps(48),
    n_kbps(56),
    n_kbps(64),
    n_kbps(80),
    n_kbps(96),//default
    n_kbps(112),
    n_kbps(128),
    n_kbps(144),
    n_kbps(160)
};
enum : int {
    MP2OptionIDVersion = 0,
    MP2OptionIDBitRateMPEG1 = 1,
    MP2OptionIDBitRateMPEG2 = 2,
};

const std::initializer_list<ExportOption> MP2Options {
    {
        MP2OptionIDVersion, XO("Version"),
        1,
        ExportOption::TypeEnum,
        { 0, 1 },
        { XO("MPEG2"), XO("MPEG1") },
    },
    {
        MP2OptionIDBitRateMPEG1, XO("Bit Rate"),
        192,
        ExportOption::TypeEnum,
        { 32, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256, 320, 384 },
        BitRateMPEG1Names
    },
    {
        MP2OptionIDBitRateMPEG2, XO("Bit Rate"),
        96,
        ExportOption::TypeEnum | ExportOption::Hidden,
        { 8, 16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128, 144, 160 },
        BitRateMPEG2Names,
    }
};
}

class MP2ExportOptionsEditor final : public ExportOptionsEditor
{
    std::vector<ExportOption> mOptions { MP2Options };
    std::unordered_map<ExportOptionID, ExportValue> mValues;
    Listener* mListener{};
public:
    MP2ExportOptionsEditor(Listener* listener);

    std::string GetName() const override;
    int GetOptionsCount() const override;
    bool GetOption(int index, ExportOption& option) const override;
    bool GetValue(ExportOptionID id, ExportValue& value) const override;
    bool SetValue(ExportOptionID id, const ExportValue& value) override;
    SampleRateList GetSampleRateList() const override;
    void Store(audacity::BasicSettings& config) const override;
    void Load(const audacity::BasicSettings& config) override;
    void OnVersionChanged();
};

class MP2ExportProcessor final : public ExportProcessor
{
    // Values taken from the twolame simple encoder sample
    constexpr static size_t pcmBufferSize = 9216 / 2; // number of samples
    constexpr static size_t mp2BufferSize = 16384u; // bytes

    struct
    {
        TranslatableString status;
        double t0;
        double t1;
        wxFileNameWrapper fName;
        std::unique_ptr<Mixer> mixer;
        ArrayOf<char> id3buffer;
        int id3len;
        twolame_options* encodeOptions{};
        std::unique_ptr<FileIO> outFile;
    } context;

public:

    ~MP2ExportProcessor() override;

    bool Initialize(AudacityProject& project, const Parameters& parameters, const wxFileNameWrapper& filename, double t0, double t1,
                    bool selectedOnly, double sampleRate, unsigned channels, MixerOptions::Downmix* mixerSpec, const Tags* tags) override;

    ExportResult Process(ExportProcessorDelegate& delegate) override;

private:
    static int AddTags(ArrayOf<char>& buffer, bool* endOfFile, const Tags* tags);
#ifdef USE_LIBID3TAG
    static void AddFrame(struct id3_tag* tp, const wxString& n, const wxString& v, const char* name);
#endif
};

class ExportMP2 final : public ExportPlugin
{
public:

    ExportMP2();

    int GetFormatCount() const override;
    FormatInfo GetFormatInfo(int) const override;

    // Required

    std::unique_ptr<ExportOptionsEditor>
    CreateOptionsEditor(int, ExportOptionsEditor::Listener*) const override;

    std::unique_ptr<ExportProcessor> CreateProcessor(int) const override;
};
