/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <sndfile.h>

#include "Mix.h"
#include "libraries/lib-import-export/ExportOptionsEditor.h"
#include <libraries/lib-import-export/ExportPlugin.h>

class ExportOptionsSFTypedEditor final : public ExportOptionsEditor
{
public:
    explicit ExportOptionsSFTypedEditor(int type);

    int GetOptionsCount() const override;
    bool GetOption(int, ExportOption& option) const override;
    bool GetValue(ExportOptionID, ExportValue& value) const override;
    bool SetValue(ExportOptionID, const ExportValue& value) override;
    SampleRateList GetSampleRateList() const override;
    void Load(const audacity::BasicSettings& config) override;
    void Store(audacity::BasicSettings& config) const override;

private:
    const int mType;
    ExportOption mEncodingOption;
    int mEncoding;
};

class PCMExportProcessor final : public ExportProcessor
{
    constexpr static size_t maxBlockLen = 44100 * 5;

    struct
    {
        int subformat;
        double t0;
        double t1;
        std::unique_ptr<Mixer> mixer;
        TranslatableString status;
        SF_INFO info;
        sampleFormat format;
        wxFile f;
        SNDFILE* sf;
        int sf_format;
        wxFileNameWrapper fName;
        int fileFormat;
        std::unique_ptr<Tags> metadata;
    } context;

public:

    PCMExportProcessor(int subformat);

    ~PCMExportProcessor() override;

    bool Initialize(AudacityProject& project, const Parameters& parameters, const wxFileNameWrapper& filename, double t0, double t1,
                    bool selectedOnly, double sampleRate, unsigned channels, MixerOptions::Downmix* mixerSpec, const Tags* tags) override;

    ExportResult Process(ExportProcessorDelegate& delegate) override;

private:

    static ArrayOf<char> AdjustString(const wxString& wxStr, int sf_format);
    static void AddStrings(SNDFILE* sf, const Tags* tags, int sf_format);
    static bool AddID3Chunk(
        const wxFileNameWrapper& fName, const Tags* tags, int sf_format);
};

class ExportPCM final : public ExportPlugin
{
public:

    ExportPCM();

    int GetFormatCount() const override;
    FormatInfo GetFormatInfo(int index) const override;

    std::vector<std::string> GetMimeTypes(int formatIndex) const override;

    // bool ParseConfig(int formatIndex, const rapidjson::Value&, ExportProcessor::Parameters& parameters) const override;

    std::unique_ptr<ExportOptionsEditor>
    CreateOptionsEditor(int, ExportOptionsEditor::Listener*) const override;

    /**
     *
     * @param format Control whether we are doing a "preset" export to a popular
     * file type, or giving the user full control over libsndfile.
     */
    std::unique_ptr<ExportProcessor> CreateProcessor(int format) const override;
};

class ExportOptionsSFEditor final : public ExportOptionsEditor
{
public:
    explicit ExportOptionsSFEditor(Listener* listener);

    int GetOptionsCount() const override;
    bool GetOption(int index, ExportOption& option) const override;
    bool GetValue(ExportOptionID id, ExportValue& value) const override;
    bool SetValue(ExportOptionID id, const ExportValue& value) override;
    SampleRateList GetSampleRateList() const override;
    void Load(const audacity::BasicSettings& config) override;
    void Store(audacity::BasicSettings& config) const override;

private:
    bool IsValidType(const ExportValue& typeValue) const;

    Listener* const mListener;
    int mType;
    std::unordered_map<int, int> mEncodings;

    std::vector<ExportOption> mOptions;
};
