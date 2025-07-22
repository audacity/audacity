#include "ExportFFmpegOptions.h"
#include "FFmpegDefines.h"
#include "Mix.h"
#include "lib-ffmpeg-support/FFmpegFunctions.h"
#include "lib-ffmpeg-support/FifoBuffer.h"
#include "libraries/lib-import-export/ExportPlugin.h"

class ExportFFmpeg final : public ExportPlugin
{
public:

    ExportFFmpeg();
    ~ExportFFmpeg() override;

    std::unique_ptr<ExportOptionsEditor>
    CreateOptionsEditor(int format, ExportOptionsEditor::Listener* listener) const override;

    int GetFormatCount() const override;
    FormatInfo GetFormatInfo(int index) const override;

    /// Callback, called from GetFilename
    bool CheckFileName(wxFileName& filename, int format = 0) const override;

    std::unique_ptr<ExportProcessor> CreateProcessor(int format) const override;

private:
    mutable std::shared_ptr<FFmpegFunctions> mFFmpeg;

    std::vector<FormatInfo> mFormatInfos;
};

class ExportOptionsFFmpegCustomEditor : public ExportOptionsEditor
{
    std::unordered_map<int, ExportValue> mValues;
    std::shared_ptr<FFmpegFunctions> mFFmpeg;
    ExportOptionsEditor::Listener* mListener{};
    //     //created on-demand
    mutable std::unique_ptr<AVCodecWrapper> mAVCodec;
public:

    ExportOptionsFFmpegCustomEditor(ExportOptionsEditor::Listener* listener = nullptr);

    int GetOptionsCount() const override;
    bool GetOption(int index, ExportOption& option) const override;
    bool GetValue(int id, ExportValue& value) const override;
    bool SetValue(int id, const ExportValue& value) override;
    SampleRateList GetSampleRateList() const override;

    void Load(const audacity::BasicSettings& config) override;
    void Store(audacity::BasicSettings& settings) const override;

// private:
    // bool CheckFFmpeg(bool showError);
    // void UpdateCodecAndFormat();
    // void OnOpen(const wxCommandEvent&);
};

///Performs actual export
class FFmpegExporter final
{
    static constexpr auto MaxAudioPacketSize { 128 * 1024 };
public:

    FFmpegExporter(std::shared_ptr<FFmpegFunctions> ffmpeg, const wxFileNameWrapper& filename, int numChannels, int subformat);

    /// Format initialization
    bool Init(const char* shortname, AudacityProject* project, int sampleRate, const Tags* metadata,
              const ExportProcessor::Parameters& parameters);

    /// Encodes audio
    bool EncodeAudioFrame(int16_t* pFrame, size_t frameSize);

    /// Flushes audio encoder
    bool Finalize();

    std::unique_ptr<Mixer> CreateMixer(
        const AudacityProject& project, bool selectionOnly, double startTime, double stopTime, MixerOptions::Downmix* mixerSpec);

private:

    /// Writes metadata
    bool AddTags(const Tags* metadata);

    /// Sets individual metadata values
    void SetMetadata(const Tags* tags, const char* name, const wxChar* tag);

    /// Check whether or not current project sample rate is compatible with the export codec
    bool CheckSampleRate(int rate, int lowrate, int highrate, const int* sampRates);

    /// Asks user to resample the project or cancel the export procedure
    int AskResample(int bitrate, int rate, int lowrate, int highrate, const int* sampRates);

    /// Codec initialization
    bool InitCodecs(int sampleRate, const ExportProcessor::Parameters& parameters);

    void WritePacket(AVPacketWrapper& packet);

    int EncodeAudio(AVPacketWrapper& pkt, int16_t* audio_samples, int nb_samples);

    std::shared_ptr<FFmpegFunctions> mFFmpeg;

    std::unique_ptr<AVOutputFormatWrapper> mEncFormatDesc;      // describes our output file to libavformat
    int mDefaultFrameSize {};
    std::unique_ptr<AVStreamWrapper> mEncAudioStream; // the output audio stream (may remain NULL)
    int mEncAudioFifoOutBufSize {};

    wxFileNameWrapper mName;

    int mSubFormat{};
    int mBitRate{};
    int mSampleRate{};
    unsigned mChannels{};
    bool mSupportsUTF8{ true };

    // Smart pointer fields, their order is the reverse in which they are reset in FreeResources():
    std::unique_ptr<FifoBuffer> mEncAudioFifo; // FIFO to write incoming audio samples into
    AVDataBuffer<int16_t> mEncAudioFifoOutBuf; // buffer to read _out_ of the FIFO into
    std::unique_ptr<AVFormatContextWrapper> mEncFormatCtx; // libavformat's context for our output file
    std::unique_ptr<AVCodecContextWrapper> mEncAudioCodecCtx;   // the encoder for the output audio stream
};

class FFmpegExportProcessor final : public ExportProcessor
{
    std::shared_ptr<FFmpegFunctions> mFFmpeg;
    struct
    {
        //same index as in GetFormatInfo, use AdjustFormatIndex to convert it to FFmpegExposedFormat
        int subformat;
        TranslatableString status;
        double t0;
        double t1;
        std::unique_ptr<Mixer> mixer;
        std::unique_ptr<FFmpegExporter> exporter;
    } context;

public:
    FFmpegExportProcessor(std::shared_ptr<FFmpegFunctions> ffmpeg, int format);

    bool Initialize(AudacityProject& project, const Parameters& parameters, const wxFileNameWrapper& filename, double t0, double t1,
                    bool selectedOnly, double sampleRate, unsigned channels, MixerOptions::Downmix* mixerSpec, const Tags* tags) override;

    ExportResult Process(ExportProcessorDelegate& delegate) override;
};
