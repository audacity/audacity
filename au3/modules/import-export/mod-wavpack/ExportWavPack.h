/*
* Audacity: A Digital Audio Editor
*/

#include "wavpack/wavpack.h"

#include "au3-mixer/Mix.h"
#include "au3-math/SampleFormat.h"
#include "au3-import-export/ExportOptionsEditor.h"
#include "au3-import-export/ExportPlugin.h"

enum : int {
    OptionIDQuality = 0,
    OptionIDBitDepth,
    OptionIDHybridMode,
    OptionIDCreateCorrection,
    OptionIDBitRate
};

const TranslatableStrings ExportQualityNames{
    TranslatableString("import-export", "Low Quality (Fast)"),
    TranslatableString("import-export", "Normal Quality"),
    TranslatableString("import-export", "High Quality (Slow)"),
    TranslatableString("import-export", "Very High Quality (Slowest)"),
};

const TranslatableStrings ExportBitDepthNames {
    TranslatableString("import-export", "16 bit"),
    TranslatableString("import-export", "24 bit"),
    TranslatableString("import-export", "32 bit float"),
};

//: bps abbreviates "bits per sample"
inline TranslatableString n_bps(int n) { return TranslatableString("import-export", "%1 bps").arg(n / 10.0); }

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
        OptionIDQuality, TranslatableString("import-export", "Quality"),
        1,
        ExportOption::TypeEnum,
        { 0, 1, 2, 3 },
        ExportQualityNames
    },
    {
        OptionIDBitDepth, TranslatableString("import-export", "Bit Depth"),
        16,
        ExportOption::TypeEnum,
        { 16, 24, 32 },
        ExportBitDepthNames
    },
    {
        OptionIDHybridMode, TranslatableString("import-export", "Hybrid Mode"),
        false,
        ExportOption::Hidden
    },
    {
        OptionIDCreateCorrection, TranslatableString("import-export", "Create Correction(.wvc) File"),
        false,
        ExportOption::ReadOnly | ExportOption::Hidden
    },
    {
        OptionIDBitRate, TranslatableString("import-export", "Bit Rate"),
        40,
        ExportOption::TypeEnum | ExportOption::Hidden,
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

    ExportOptionsWavPackEditor(Listener* listener);

    std::string GetName() const override;
    int GetOptionsCount() const override;
    bool GetOption(int index, ExportOption& option) const override;
    bool GetValue(ExportOptionID id, ExportValue& value) const override;
    bool SetValue(ExportOptionID id, const ExportValue& value) override;
    SampleRateList GetSampleRateList() const override;
    void Load(const audacity::BasicSettings& config) override;
    void Store(audacity::BasicSettings& config) const override;

private:
    void OnHybridModeChange(bool hybridMode);
};

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

    bool ParseConfig(int formatIndex, const std::string& config, ExportProcessor::Parameters& parameters) const override;

    std::unique_ptr<ExportOptionsEditor>
    CreateOptionsEditor(int, ExportOptionsEditor::Listener*) const override;

    std::unique_ptr<ExportProcessor> CreateProcessor(int format) const override;
};
