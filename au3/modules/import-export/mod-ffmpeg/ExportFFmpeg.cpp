/**********************************************************************

   Audacity: A Digital Audio Editor

   ExportFFmpeg.cpp

   Audacity(R) is copyright (c) 1999-2009 Audacity Team.
   License: GPL v2 or later.  See License.txt.

   LRN

******************************************************************//**

\class ExportFFmpeg
\brief Controlling class for FFmpeg exporting.  Creates the options
dialog of the appropriate type, adds tags and invokes the export
function.

*//*******************************************************************/

#include "../FFmpeg.h"
#include "FFmpegFunctions.h"
#include "FifoBuffer.h"

#include <wx/app.h>
#include <wx/log.h>

#include <wx/window.h>
#include <wx/button.h>
#include <wx/textctrl.h>

#include "BasicSettings.h"
#include "Mix.h"
#include "Tags.h"
#include "Track.h"
#include "wxFileNameWrapper.h"

#include "ExportFFmpegOptions.h"
#include "SelectFile.h"
#include "ShuttleGui.h"

#include "ExportPluginHelpers.h"
#include "PlainExportOptionsEditor.h"
#include "FFmpegDefines.h"
#include "ExportOptionsUIServices.h"
#include "ExportPluginRegistry.h"

#if defined(WIN32) && _MSC_VER < 1900
#define snprintf _snprintf
#endif

// Define this to automatically resample audio to the nearest supported sample rate
#define FFMPEG_AUTO_RESAMPLE 1

static int AdjustFormatIndex(int format)
{
    int subFormat = -1;
    for (int i = 0; i <= FMT_OTHER; i++) {
        if (ExportFFmpegOptions::fmts[i].compiledIn) {
            subFormat++;
        }
        if (subFormat == format || i == FMT_OTHER) {
            subFormat = i;
            break;
        }
    }
    return subFormat;
}

namespace {
const int iAC3SampleRates[] =
{ 32000, 44100, 48000, 0 };

const int iWMASampleRates[] =
{ 8000, 11025, 16000, 22050, 44100, 0 };

///\param rates 0-terminated array
ExportOptionsEditor::SampleRateList ToSampleRateList(const int* rates)
{
    ExportOptionsEditor::SampleRateList list;
    int index = 0;
    while (rates[index] != 0) {
        list.push_back(rates[index++]);
    }
    return list;
}

// i18n-hint kbps abbreviates "thousands of bits per second"
TranslatableString n_kbps(int n) { return XO("%d kbps").Format(n); }
TranslatableString f_kbps(double d) { return XO("%.2f kbps").Format(d); }

enum : int
{
    AC3OptionIDBitRate = 0
};

const std::initializer_list<PlainExportOptionsEditor::OptionDesc> AC3Options {
    {
        {
            AC3OptionIDBitRate, XO("Bit Rate"),
            160000,
            ExportOption::TypeEnum,
            {
                32000,
                40000,
                48000,
                56000,
                64000,
                80000,
                96000,
                112000,
                128000,
                160000,
                192000,
                224000,
                256000,
                320000,
                384000,
                448000,
                512000,
                576000,
                640000
            },
            {
                n_kbps(32),
                n_kbps(40),
                n_kbps(48),
                n_kbps(56),
                n_kbps(64),
                n_kbps(80),
                n_kbps(96),
                n_kbps(112),
                n_kbps(128),
                n_kbps(160),
                n_kbps(192),
                n_kbps(224),
                n_kbps(256),
                n_kbps(320),
                n_kbps(384),
                n_kbps(448),
                n_kbps(512),
                n_kbps(576),
                n_kbps(640),
            }
        }, wxT("/FileFormats/AC3BitRate")
    }
};

enum : int
{
    AACOptionIDQuality = 0
};

//NB: user-entered values for AAC are not always followed; mono is clamped to 98-160, stereo 196-320
const std::initializer_list<PlainExportOptionsEditor::OptionDesc> AACOptions {
    {
        {
            AACOptionIDQuality, XO("Quality (kbps)"),
            256,
            ExportOption::TypeRange,
            { 98, 320 }
        }, wxT("/FileFormats/AACQuality")
    }
};

enum : int
{
    AMRNBOptionIDBitRate = 0
};

const std::initializer_list<PlainExportOptionsEditor::OptionDesc> AMRNBOptions {
    {
        {
            AMRNBOptionIDBitRate, XO("Bit Rate"),
            12200,
            ExportOption::TypeEnum,
            {
                4750,
                5150,
                5900,
                6700,
                7400,
                7950,
                10200,
                12200,
            },
            {
                f_kbps(4.75),
                f_kbps(5.15),
                f_kbps(5.90),
                f_kbps(6.70),
                f_kbps(7.40),
                f_kbps(7.95),
                f_kbps(10.20),
                f_kbps(12.20),
            }
        }, wxT("/FileFormats/AMRNBBitRate")
    }
};

#ifdef SHOW_FFMPEG_OPUS_EXPORT
enum : int
{
    OPUSOptionIDBitRate = 0,
    OPUSOptionIDCompression,
    OPUSOptionIDFrameDuration,
    OPUSOptionIDVBRMode,
    OPUSOptionIDApplication,
    OPUSOptionIDCutoff
};

const std::initializer_list<PlainExportOptionsEditor::OptionDesc> OPUSOptions {
    {
        {
            OPUSOptionIDBitRate, XO("Bit Rate"),
            128000,
            ExportOption::TypeEnum,
            {
                6000,
                8000,
                16000,
                24000,
                32000,
                40000,
                48000,
                64000,
                80000,
                96000,
                128000,
                160000,
                192000,
                256000
            },
            {
                n_kbps(6),
                n_kbps(8),
                n_kbps(16),
                n_kbps(24),
                n_kbps(32),
                n_kbps(40),
                n_kbps(48),
                n_kbps(64),
                n_kbps(80),
                n_kbps(96),
                n_kbps(128),
                n_kbps(160),
                n_kbps(192),
                n_kbps(256),
            }
        }, wxT("/FileFormats/OPUSBitrate")
    },
    {
        {
            OPUSOptionIDCompression, XO("Compression"),
            10,
            ExportOption::TypeRange,
            { 0, 10 }
        }, wxT("/FileFormats/OPUSCompression")
    },
    {
        {
            OPUSOptionIDFrameDuration, XO("Frame Duration"),
            std::string("20"),
            ExportOption::TypeEnum,
            {
                std::string("2.5"),
                std::string("5"),
                std::string("10"),
                std::string("20"),
                std::string("40"),
                std::string("60")
            },
            {
                XO("2.5 ms"),
                XO("5 ms"),
                XO("10 ms"),
                XO("20 ms"),
                XO("40 ms"),
                XO("60 ms"),
            }
        }, wxT("/FileFormats/OPUSFrameDuration")
    },
    {
        {
            OPUSOptionIDVBRMode, XO("Vbr Mode"),
            std::string("on"),
            ExportOption::TypeEnum,
            { std::string("off"), std::string("on"), std::string("constrained") },
            { XO("Off"), XO("On"), XO("Constrained") }
        }, wxT("/FileFormats/OPUSVbrMode")
    },
    {
        {
            OPUSOptionIDApplication, XO("Application"),
            std::string("audio"),
            ExportOption::TypeEnum,
            { std::string("voip"), std::string("audio"), std::string("lowdelay") },
            { XO("VOIP"), XO("Audio"), XO("Low Delay") }
        }, wxT("/FileFormats/OPUSApplication")
    },
    {
        {
            OPUSOptionIDCutoff, XO("Cutoff"),
            std::string("0"),
            ExportOption::TypeEnum,
            {
                std::string("0"),
                std::string("4000"),
                std::string("6000"),
                std::string("8000"),
                std::string("12000"),
                std::string("20000")
            },
            {
                XO("Disabled"),
                XO("Narrowband"),
                XO("Mediumband"),
                XO("Wideband"),
                XO("Super Wideband"),
                XO("Fullband")
            }
        }, wxT("/FileFormats/OPUSCutoff")
    },
};
#endif

enum : int
{
    WMAOptionIDBitRate = 0
};

const std::initializer_list<PlainExportOptionsEditor::OptionDesc> WMAOptions {
    {
        {
            WMAOptionIDBitRate, XO("Bit Rate"),
            128000,
            ExportOption::TypeEnum,
            {
                24000,
                32000,
                40000,
                48000,
                64000,
                80000,
                96000,
                128000,
                160000,
                192000,
                256000,
                320000
            },
            {
                n_kbps(24),
                n_kbps(32),
                n_kbps(40),
                n_kbps(48),
                n_kbps(64),
                n_kbps(80),
                n_kbps(96),
                n_kbps(128),
                n_kbps(160),
                n_kbps(192),
                n_kbps(256),
                n_kbps(320),
            }
        }, wxT("/FileFormats/WMABitRate")
    }
};

const std::vector<ExportOption> FFmpegOptions {
    { FELanguageID, {}, std::string() },
    { FESampleRateID, {}, 0 },
    { FEBitrateID, {}, 0 },
    { FETagID, {}, std::string() },
    { FEQualityID, {}, 0 },
    { FECutoffID, {}, 0 },
    { FEBitReservoirID, {}, true },
    { FEVariableBlockLenID, {}, true },
    { FECompLevelID, {}, -1 },
    { FEFrameSizeID, {}, 0 },
    { FELPCCoeffsID, {}, 0 },
    { FEMinPredID, {}, -1 },
    { FEMaxPredID, {}, -1 },
    { FEMinPartOrderID, {}, -1 },
    { FEMaxPartOrderID, {}, -1 },
    { FEPredOrderID, {}, 0 },
    { FEMuxRateID, {}, 0 },
    { FEPacketSizeID, {}, 0 },
    { FECodecID, {}, std::string() },
    { FEFormatID, {}, std::string() }
};

class ExportOptionsFFmpegCustomEditor : public ExportOptionsEditor, public ExportOptionsUIServices
{
    std::unordered_map<int, ExportValue> mValues;
    std::shared_ptr<FFmpegFunctions> mFFmpeg;
    ExportOptionsEditor::Listener* mListener{};
    //created on-demand
    mutable std::unique_ptr<AVCodecWrapper> mAVCodec;
public:

    ExportOptionsFFmpegCustomEditor(ExportOptionsEditor::Listener* listener = nullptr)
        : mListener(listener)
    {
    }

    void PopulateUI(ShuttleGui& S) override
    {
        CheckFFmpeg(true);
        //Continue anyway, as we do not need ffmpeg functions to build and fill in the UI

        mParent = S.GetParent();

        S.StartHorizontalLay(wxCENTER);
        {
            S.StartVerticalLay(wxCENTER, 0);
            {
                S.AddButton(XXO("Open custom FFmpeg format options"))
                ->Bind(wxEVT_BUTTON, &ExportOptionsFFmpegCustomEditor::OnOpen, this);
                S.StartMultiColumn(2, wxCENTER);
                {
                    S.AddPrompt(XXO("Current Format:"));
                    mFormat = S.Name(XXO("Current Format:"))
                              .Style(wxTE_READONLY).AddTextBox({}, wxT(""), 25);
                    S.AddPrompt(XXO("Current Codec:"));
                    mCodec = S.Name(XXO("Current Codec:"))
                             .Style(wxTE_READONLY).AddTextBox({}, wxT(""), 25);
                }
                S.EndMultiColumn();
            }
            S.EndHorizontalLay();
        }
        S.EndHorizontalLay();

        UpdateCodecAndFormat();
    }

    bool TransferDataFromWindow() override
    {
        Load(*gPrefs);
        return true;
    }

    int GetOptionsCount() const override
    {
        return static_cast<int>(FFmpegOptions.size());
    }

    bool GetOption(int index, ExportOption& option) const override
    {
        if (index >= 0 && index < FFmpegOptions.size()) {
            option = FFmpegOptions[index];
            return true;
        }
        return false;
    }

    bool GetValue(int id, ExportValue& value) const override
    {
        auto it = mValues.find(id);
        if (it != mValues.end()) {
            value = it->second;
            return true;
        }
        return false;
    }

    bool SetValue(int id, const ExportValue& value) override
    {
        return false;
    }

    SampleRateList GetSampleRateList() const override
    {
        if (!mAVCodec) {
            auto it = mValues.find(FECodecID);
            if (it == mValues.end()) {
                return {}
            }

            const auto codecId = *std::get_if<std::string>(&it->second);
            if (mFFmpeg) {
                mAVCodec = mFFmpeg->CreateEncoder(codecId.c_str());
            }
        }
        if (!mAVCodec) {
            return {}
        }

        if (const auto rates = mAVCodec->GetSupportedSamplerates()) {
            return ToSampleRateList(rates);
        }
        return {};
    }

    void Load(const audacity::BasicSettings& config) override
    {
        mValues[FELanguageID] = std::string(config.Read(wxT("/FileFormats/FFmpegLanguage"), wxT("")).ToUTF8());
        mValues[FESampleRateID] = static_cast<int>(config.Read(wxT("/FileFormats/FFmpegSampleRate"), 0L));
        mValues[FEBitrateID] = static_cast<int>(config.Read(wxT("/FileFormats/FFmpegBitRate"), 0L));
        mValues[FETagID] = std::string(config.Read(wxT("/FileFormats/FFmpegTag"), wxT(""))
                                       .mb_str(wxConvUTF8));
        mValues[FEQualityID] = static_cast<int>(config.Read(wxT("/FileFormats/FFmpegQuality"), -99999L));
        mValues[FECutoffID] = static_cast<int>(config.Read(wxT("/FileFormats/FFmpegCutOff"), 0L));
        mValues[FEBitReservoirID] = config.ReadBool(wxT("/FileFormats/FFmpegBitReservoir"), true);
        mValues[FEVariableBlockLenID] = config.ReadBool(wxT("/FileFormats/FFmpegVariableBlockLen"), true);
        mValues[FECompLevelID] = static_cast<int>(config.Read(wxT("/FileFormats/FFmpegCompLevel"), -1L));
        mValues[FEFrameSizeID] = static_cast<int>(config.Read(wxT("/FileFormats/FFmpegFrameSize"), 0L));

        mValues[FELPCCoeffsID] = static_cast<int>(config.Read(wxT("/FileFormats/FFmpegLPCCoefPrec"), 0L));
        mValues[FEMinPredID] = static_cast<int>(config.Read(wxT("/FileFormats/FFmpegMinPredOrder"), -1L));
        mValues[FEMaxPredID] = static_cast<int>(config.Read(wxT("/FileFormats/FFmpegMaxPredOrder"), -1L));
        mValues[FEMinPartOrderID] = static_cast<int>(config.Read(wxT("/FileFormats/FFmpegMinPartOrder"), -1L));
        mValues[FEMaxPartOrderID] = static_cast<int>(config.Read(wxT("/FileFormats/FFmpegMaxPartOrder"), -1L));
        mValues[FEPredOrderID] = static_cast<int>(config.Read(wxT("/FileFormats/FFmpegPredOrderMethod"), 0L));
        mValues[FEMuxRateID] = static_cast<int>(config.Read(wxT("/FileFormats/FFmpegMuxRate"), 0L));
        mValues[FEPacketSizeID] = static_cast<int>(config.Read(wxT("/FileFormats/FFmpegPacketSize"), 0L));
        mValues[FECodecID] = std::string(config.Read(wxT("/FileFormats/FFmpegCodec")));
        mValues[FEFormatID] = std::string(config.Read(wxT("/FileFormats/FFmpegFormat")));
    }

    void Store(audacity::BasicSettings& settings) const override
    {
    }

private:

    bool CheckFFmpeg(bool showError)
    {
        // Show "Locate FFmpeg" dialog
        if (!mFFmpeg) {
            mFFmpeg = FFmpegFunctions::Load();
            if (!mFFmpeg) {
                FindFFmpegLibs();
                return LoadFFmpeg(showError);
            }
        }
        return true;
    }

    void UpdateCodecAndFormat()
    {
        mFormat->SetValue(gPrefs->Read(wxT("/FileFormats/FFmpegFormat"), wxT("")));
        mCodec->SetValue(gPrefs->Read(wxT("/FileFormats/FFmpegCodec"), wxT("")));
    }

    void OnOpen(const wxCommandEvent&)
    {
        if (!CheckFFmpeg(true)) {
            return;
        }

   #ifdef __WXMAC__
        // Bug 2077 Must be a parent window on OSX or we will appear behind.
        auto pWin = wxGetTopLevelParent(mParent);
   #else
        // Use GetTopWindow on windows as there is no hWnd with top level parent.
        auto pWin = wxTheApp->GetTopWindow();
   #endif

        ExportFFmpegOptions od(pWin);
        od.ShowModal();
        //ExportFFmpegOptions uses gPrefs to store options
        //Instead we could provide it with instance of wxConfigBase
        //constructed locally and read from it later
        Load(*gPrefs);
        mAVCodec.reset();

        UpdateCodecAndFormat();
        if (mListener) {
            mListener->OnSampleRateListChange();
        }
    }

    wxWindow* mParent { nullptr };
    wxTextCtrl* mFormat { nullptr };
    wxTextCtrl* mCodec { nullptr };
};
}

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

FFmpegExporter::FFmpegExporter(std::shared_ptr<FFmpegFunctions> ffmpeg,
                               const wxFileNameWrapper& filename,
                               int numChannels,
                               int subFormat)
    : mFFmpeg(std::move(ffmpeg))
    , mName(filename)
    , mChannels(numChannels)
    , mSubFormat(subFormat)
{
    if (!mFFmpeg) {
        mFFmpeg = FFmpegFunctions::Load();
    }
}

std::unique_ptr<Mixer> FFmpegExporter::CreateMixer(
    const AudacityProject& project, bool selectionOnly, double startTime,
    double stopTime, MixerOptions::Downmix* mixerSpec)
{
    return ExportPluginHelpers::CreateMixer(
        project, selectionOnly, startTime, stopTime, mChannels, mDefaultFrameSize,
        true, mSampleRate, int16Sample, mixerSpec);
}

ExportFFmpeg::ExportFFmpeg()
{
    mFFmpeg = FFmpegFunctions::Load();

    int avfver = mFFmpeg ? mFFmpeg->AVFormatVersion.GetIntVersion() : 0;

    int newfmt;
    // Adds export types from the export type list
    for (newfmt = 0; newfmt < FMT_LAST; newfmt++) {
        wxString shortname(ExportFFmpegOptions::fmts[newfmt].shortname);
        // Don't hide export types when there's no av-libs, and don't hide FMT_OTHER
        if (newfmt < FMT_OTHER && mFFmpeg) {
            // Format/Codec support is compiled in?
            auto avoformat = mFFmpeg->GuessOutputFormat(shortname.mb_str(), nullptr, nullptr);
            auto avcodec = mFFmpeg->CreateEncoder(mFFmpeg->GetAVCodecID(ExportFFmpegOptions::fmts[newfmt].codecid));

            if (avoformat == NULL || avcodec == NULL) {
                ExportFFmpegOptions::fmts[newfmt].compiledIn = false;
                continue;
            }
        }
        FormatInfo formatInfo {};
        formatInfo.format = ExportFFmpegOptions::fmts[newfmt].name;
        formatInfo.extensions.push_back(ExportFFmpegOptions::fmts[newfmt].extension);
        // For some types add other extensions
        switch (newfmt) {
        case FMT_M4A:
            formatInfo.extensions.push_back(wxT("3gp"));
            formatInfo.extensions.push_back(wxT("m4r"));
            formatInfo.extensions.push_back(wxT("mp4"));
            break;
        case FMT_WMA2:
            formatInfo.extensions.push_back(wxT("asf"));
            formatInfo.extensions.push_back(wxT("wmv"));
            break;
        default:
            break;
        }
        formatInfo.maxChannels = ExportFFmpegOptions::fmts[newfmt].maxchannels;
        formatInfo.description = ExportFFmpegOptions::fmts[newfmt].description;

        const int canmeta = ExportFFmpegOptions::fmts[newfmt].canmetadata;
        formatInfo.canMetaData = canmeta && (canmeta == AV_CANMETA || canmeta <= avfver);

        mFormatInfos.push_back(std::move(formatInfo));
    }
}

ExportFFmpeg::~ExportFFmpeg() = default;

std::unique_ptr<ExportOptionsEditor>
ExportFFmpeg::CreateOptionsEditor(int format, ExportOptionsEditor::Listener* listener) const
{
    switch (AdjustFormatIndex(format)) {
    case FMT_M4A:
        return std::make_unique<PlainExportOptionsEditor>(AACOptions, listener);
    case FMT_AC3:
        return std::make_unique<PlainExportOptionsEditor>(
            AC3Options,
            ToSampleRateList(iAC3SampleRates),
            listener);
    case FMT_AMRNB:
        return std::make_unique<PlainExportOptionsEditor>(
            AMRNBOptions,
            ExportOptionsEditor::SampleRateList { 8000 },
            listener);
#ifdef SHOW_FFMPEG_OPUS_EXPORT
    case FMT_OPUS:
        return std::make_unique<PlainExportOptionsEditor>(OPUSOptions, listener);
#endif
    case FMT_WMA2:
        return std::make_unique<PlainExportOptionsEditor>(
            WMAOptions,
            ToSampleRateList(iWMASampleRates),
            listener);
    case FMT_OTHER:
        return std::make_unique<ExportOptionsFFmpegCustomEditor>(listener);
    }
    return {};
}

int ExportFFmpeg::GetFormatCount() const
{
    return static_cast<int>(mFormatInfos.size());
}

FormatInfo ExportFFmpeg::GetFormatInfo(int index) const
{
    if (index >= 0 && index < mFormatInfos.size()) {
        return mFormatInfos[index];
    }
    return mFormatInfos[FMT_OTHER];
}

bool ExportFFmpeg::CheckFileName(wxFileName& WXUNUSED(filename), int WXUNUSED(format)) const
{
    bool result = true;

    // Show "Locate FFmpeg" dialog
    mFFmpeg = FFmpegFunctions::Load();
    if (!mFFmpeg) {
        FindFFmpegLibs();
        mFFmpeg = FFmpegFunctions::Load();

        return LoadFFmpeg(true);
    }

    return result;
}

std::unique_ptr<ExportProcessor> ExportFFmpeg::CreateProcessor(int format) const
{
    return std::make_unique<FFmpegExportProcessor>(mFFmpeg, format);
}

bool FFmpegExporter::Init(const char* shortname,
                          AudacityProject* project,
                          int sampleRate,
                          const Tags* metadata,
                          const ExportProcessor::Parameters& parameters)
{
    if (!mFFmpeg) {
        return false;
    }

    // See if libavformat has modules that can write our output format. If so, mEncFormatDesc
    // will describe the functions used to write the format (used internally by libavformat)
    // and the default video/audio codecs that the format uses.
    const auto path = mName.GetFullPath();
    if ((mEncFormatDesc = mFFmpeg->GuessOutputFormat(shortname, OSINPUT(path), nullptr)) == nullptr) {
        throw ExportException(_("FFmpeg : ERROR - Can't determine format description for file \"%s\".").Format(path));
    }

    // mEncFormatCtx is used by libavformat to carry around context data re our output file.
    mEncFormatCtx = mFFmpeg->CreateAVFormatContext();
    if (!mEncFormatCtx) {
        throw ExportException(_("FFmpeg : ERROR - Can't allocate output format context."));
    }

    // Initialise the output format context.
    mEncFormatCtx->SetOutputFormat(mFFmpeg->CreateAVOutputFormatWrapper(mEncFormatDesc->GetWrappedValue()));
    mEncFormatCtx->SetFilename(OSINPUT(path));

    // At the moment Audacity can export only one audio stream
    if ((mEncAudioStream = mEncFormatCtx->CreateStream()) == nullptr) {
        throw ExportException(_("FFmpeg : ERROR - Can't add audio stream to output file \"%s\"."));
    }

    // Documentation for avformat_new_stream says
    // "User is required to call avcodec_close() and avformat_free_context() to clean
    // up the allocation by avformat_new_stream()."

    // We use smart pointers that ensure these cleanups either in their destructors or
    // sooner if they are reset.  These are std::unique_ptr with nondefault deleter
    // template parameters.

    // mEncFormatCtx takes care of avformat_free_context(), so
    // mEncAudioStream can be a plain pointer.

    // mEncAudioCodecCtx now becomes responsible for closing the codec:
    mEncAudioCodecCtx = mEncAudioStream->GetAVCodecContext();
    mEncAudioStream->SetId(0);

    // Open the output file.
    if (!(mEncFormatDesc->GetFlags() & AUDACITY_AVFMT_NOFILE)) {
        AVIOContextWrapper::OpenResult result
            =mEncFormatCtx->OpenOutputContext(path);

        if (result != AVIOContextWrapper::OpenResult::Success) {
            throw ExportException(_("FFmpeg : ERROR - Can't open output file \"%s\" to write. Error code is %d.")
                                  .Format(path, static_cast<int>(result)));
        }
    }

    // Open the audio stream's codec and initialise any stream related data.
    if (!InitCodecs(sampleRate, parameters)) {
        return false;
    }

    if (mEncAudioStream->SetParametersFromContext(*mEncAudioCodecCtx) < 0) {
        return false;
    }

    if (metadata == NULL) {
        metadata = &Tags::Get(*project);
    }

    // Add metadata BEFORE writing the header.
    // At the moment that works with ffmpeg-git and ffmpeg-0.5 for MP4.
    const auto canmeta = ExportFFmpegOptions::fmts[mSubFormat].canmetadata;
    const auto avfver = mFFmpeg->AVFormatVersion.GetIntVersion();
    if (canmeta && (canmeta == AV_CANMETA || canmeta <= avfver)) {
        mSupportsUTF8 = ExportFFmpegOptions::fmts[mSubFormat].canutf8;
        AddTags(metadata);
    }

    // Write headers to the output file.
    int err
        =mFFmpeg->avformat_write_header(mEncFormatCtx->GetWrappedValue(), nullptr);

    if (err < 0) {
        throw ExportException(XO("FFmpeg : ERROR - Can't write headers to output file \"%s\". Error code is %d.")
                              .Format(path, err)
                              .Translation());
    }

    return true;
}

bool FFmpegExporter::CheckSampleRate(int rate, int lowrate, int highrate, const int* sampRates)
{
    if (lowrate && highrate) {
        if (rate < lowrate || rate > highrate) {
            return false;
        }
    }

    if (sampRates) {
        for (int i = 0; sampRates[i] > 0; i++) {
            if (rate == sampRates[i]) {
                return true;
            }
        }
    }

    return false;
}

bool FFmpegExporter::InitCodecs(int sampleRate,
                                const ExportProcessor::Parameters& parameters)
{
    std::unique_ptr<AVCodecWrapper> codec;

    AVDictionaryWrapper options(*mFFmpeg);

    // Get the sample rate from the passed settings if we haven't set it before.
    // Doing this only when not set allows us to carry the sample rate from one
    // iteration of ExportMultiple to the next.  This prevents multiple resampling
    // dialogs in the event the codec can't support the specified rate.
    if (!mSampleRate) {
        //TODO: Does not work with export multiple any more...
        mSampleRate = sampleRate;
    }

    // Configure the audio stream's codec context.

    const auto codecID = ExportFFmpegOptions::fmts[mSubFormat].codecid;

    mEncAudioCodecCtx->SetGlobalQuality(-99999); //quality mode is off by default;

    // Each export type has its own settings
    switch (mSubFormat) {
    case FMT_M4A:
    {
        int q = ExportPluginHelpers::GetParameterValue(parameters, AACOptionIDQuality, -99999);

        q = wxClip(q, 98 * mChannels, 160 * mChannels);
        // Set bit rate to between 98 kbps and 320 kbps (if two channels)
        mEncAudioCodecCtx->SetBitRate(q * 1000);
        mEncAudioCodecCtx->SetProfile(AUDACITY_FF_PROFILE_AAC_LOW);
        mEncAudioCodecCtx->SetCutoff(0);

        break;
    }
    case FMT_AC3:
        mEncAudioCodecCtx->SetBitRate(ExportPluginHelpers::GetParameterValue(parameters, AC3OptionIDBitRate, 192000));
        if (!CheckSampleRate(
                mSampleRate, iAC3SampleRates[0],
                iAC3SampleRates[2],
                &iAC3SampleRates[0])) {
            mSampleRate = AskResample(
                mEncAudioCodecCtx->GetBitRate(), mSampleRate,
                iAC3SampleRates[0],
                iAC3SampleRates[2],
                &iAC3SampleRates[0]);
        }
        break;
    case FMT_AMRNB:
        mSampleRate = 8000;
        mEncAudioCodecCtx->SetBitRate(ExportPluginHelpers::GetParameterValue(parameters, AMRNBOptionIDBitRate, 12200));
        break;
#ifdef SHOW_FFMPEG_OPUS_EXPORT
    case FMT_OPUS:
        options.Set("b", ExportPluginHelpers::GetParameterValue<std::string>(parameters, OPUSOptionIDBitRate, "128000"), 0);
        options.Set("vbr", ExportPluginHelpers::GetParameterValue<std::string>(parameters, OPUSOptionIDVBRMode, "on"), 0);
        options.Set("compression_level", ExportPluginHelpers::GetParameterValue<std::string>(parameters, OPUSOptionIDCompression, "10"), 0);
        options.Set("frame_duration", ExportPluginHelpers::GetParameterValue<std::string>(parameters, OPUSOptionIDFrameDuration, "20"), 0);
        options.Set("application", ExportPluginHelpers::GetParameterValue<std::string>(parameters, OPUSOptionIDApplication, "audio"), 0);
        options.Set("cutoff", ExportPluginHelpers::GetParameterValue<std::string>(parameters, OPUSOptionIDCutoff, "0"), 0);
        options.Set("mapping_family", mChannels <= 2 ? "0" : "255", 0);
        break;
#endif
    case FMT_WMA2:
        mEncAudioCodecCtx->SetBitRate(ExportPluginHelpers::GetParameterValue(parameters, WMAOptionIDBitRate, 198000));
        if (!CheckSampleRate(
                mSampleRate, iWMASampleRates[0],
                iWMASampleRates[4],
                &iWMASampleRates[0])) {
            mSampleRate = AskResample(
                mEncAudioCodecCtx->GetBitRate(), mSampleRate,
                iWMASampleRates[0],
                iWMASampleRates[4],
                &iWMASampleRates[0]);
        }
        break;
    case FMT_OTHER:
    {
        AVDictionaryWrapper streamMetadata = mEncAudioStream->GetMetadata();
        streamMetadata.Set(
            "language",
            ExportPluginHelpers::GetParameterValue<std::string>(parameters, FELanguageID), 0);

        mEncAudioStream->SetMetadata(streamMetadata);

        mEncAudioCodecCtx->SetSampleRate(
            ExportPluginHelpers::GetParameterValue(parameters, FESampleRateID, 0));

        if (mEncAudioCodecCtx->GetSampleRate() != 0) {
            mSampleRate = mEncAudioCodecCtx->GetSampleRate();
        }

        mEncAudioCodecCtx->SetBitRate(
            ExportPluginHelpers::GetParameterValue(parameters, FEBitrateID, 0));

        mEncAudioCodecCtx->SetCodecTagFourCC(
            ExportPluginHelpers::GetParameterValue<std::string>(parameters, FETagID).c_str());

        mEncAudioCodecCtx->SetGlobalQuality(
            ExportPluginHelpers::GetParameterValue(parameters, FEQualityID, -99999));
        mEncAudioCodecCtx->SetCutoff(
            ExportPluginHelpers::GetParameterValue(parameters, FECutoffID, 0));
        mEncAudioCodecCtx->SetFlags2(0);

        if (ExportPluginHelpers::GetParameterValue(parameters, FEBitReservoirID, true)) {
            options.Set("reservoir", "1", 0);
        }

        if (ExportPluginHelpers::GetParameterValue(parameters, FEVariableBlockLenID, true)) {
            mEncAudioCodecCtx->SetFlags2(
                mEncAudioCodecCtx->GetFlags2() | 0x0004); // WMA only?
        }
        mEncAudioCodecCtx->SetCompressionLevel(
            ExportPluginHelpers::GetParameterValue(parameters, FECompLevelID, -1));
        mEncAudioCodecCtx->SetFrameSize(
            ExportPluginHelpers::GetParameterValue(parameters, FEFrameSizeID, 0));

        // FIXME The list of supported options for the selected encoder should be
        // extracted instead of a few hardcoded

        options.Set(
            "lpc_coeff_precision",
            ExportPluginHelpers::GetParameterValue(parameters, FELPCCoeffsID, 0));
        options.Set(
            "min_prediction_order",
            ExportPluginHelpers::GetParameterValue(parameters, FEMinPredID, -1));
        options.Set(
            "max_prediction_order",
            ExportPluginHelpers::GetParameterValue(parameters, FEMaxPredID, -1));
        options.Set(
            "min_partition_order",
            ExportPluginHelpers::GetParameterValue(parameters, FEMinPartOrderID, -1));
        options.Set(
            "max_partition_order",
            ExportPluginHelpers::GetParameterValue(parameters, FEMaxPartOrderID, -1));
        options.Set(
            "prediction_order_method",
            ExportPluginHelpers::GetParameterValue(parameters, FEPredOrderID, 0));
        options.Set(
            "muxrate",
            ExportPluginHelpers::GetParameterValue(parameters, FEMuxRateID, 0));

        mEncFormatCtx->SetPacketSize(
            ExportPluginHelpers::GetParameterValue(parameters, FEPacketSizeID, 0));

        codec = mFFmpeg->CreateEncoder(
            ExportPluginHelpers::GetParameterValue<std::string>(parameters, FECodecID).c_str());

        if (!codec) {
            codec = mFFmpeg->CreateEncoder(mEncFormatDesc->GetAudioCodec());
        }
    }
    break;
    default:
        return false;
    }

    // This happens if user refused to resample the project
    if (mSampleRate == 0) {
        return false;
    }

    if (mEncAudioCodecCtx->GetGlobalQuality() >= 0) {
        mEncAudioCodecCtx->SetFlags(
            mEncAudioCodecCtx->GetFlags() | AUDACITY_AV_CODEC_FLAG_QSCALE);
    } else {
        mEncAudioCodecCtx->SetGlobalQuality(0);
    }

    mEncAudioCodecCtx->SetGlobalQuality(mEncAudioCodecCtx->GetGlobalQuality() * AUDACITY_FF_QP2LAMBDA);
    mEncAudioCodecCtx->SetSampleRate(mSampleRate);
    mEncAudioCodecCtx->SetChannelLayout(mFFmpeg->CreateDefaultChannelLayout(mChannels).get());
    mEncAudioCodecCtx->SetTimeBase({ 1, mSampleRate });
    mEncAudioCodecCtx->SetSampleFmt(static_cast<AVSampleFormatFwd>(AUDACITY_AV_SAMPLE_FMT_S16));
    mEncAudioCodecCtx->SetStrictStdCompliance(
        AUDACITY_FF_COMPLIANCE_EXPERIMENTAL);

    if (codecID == AUDACITY_AV_CODEC_ID_AC3) {
        // As of Jan 4, 2011, the default AC3 encoder only accept SAMPLE_FMT_FLT samples.
        // But, currently, Audacity only supports SAMPLE_FMT_S16.  So, for now, look for the
        // "older" AC3 codec.  this is not a proper solution, but will suffice until other
        // encoders no longer support SAMPLE_FMT_S16.
        codec = mFFmpeg->CreateEncoder("ac3_fixed");
    }

    if (!codec) {
        codec = mFFmpeg->CreateEncoder(mFFmpeg->GetAVCodecID(codecID));
    }

    // Is the required audio codec compiled into libavcodec?
    if (codec == NULL) {
        /* i18n-hint: "codec" is short for a "coder-decoder" algorithm */
        throw ExportException(XO("FFmpeg cannot find audio codec 0x%x.\nSupport for this codec is probably not compiled in.")
                              .Format(static_cast<const unsigned int>(codecID.value))
                              .Translation());
    }

    if (codec->GetSampleFmts()) {
        for (int i = 0; codec->GetSampleFmts()[i] != AUDACITY_AV_SAMPLE_FMT_NONE; i++) {
            AVSampleFormatFwd fmt = codec->GetSampleFmts()[i];

            if (
                fmt == AUDACITY_AV_SAMPLE_FMT_U8
                || fmt == AUDACITY_AV_SAMPLE_FMT_U8P
                || fmt == AUDACITY_AV_SAMPLE_FMT_S16
                || fmt == AUDACITY_AV_SAMPLE_FMT_S16P
                || fmt == AUDACITY_AV_SAMPLE_FMT_S32
                || fmt == AUDACITY_AV_SAMPLE_FMT_S32P
                || fmt == AUDACITY_AV_SAMPLE_FMT_FLT
                || fmt == AUDACITY_AV_SAMPLE_FMT_FLTP) {
                mEncAudioCodecCtx->SetSampleFmt(fmt);
            }

            if (
                fmt == AUDACITY_AV_SAMPLE_FMT_S16
                || fmt == AUDACITY_AV_SAMPLE_FMT_S16P) {
                break;
            }
        }
    }

    if (codec->GetSupportedSamplerates()) {
        // Workaround for crash in bug #2378.  Proper fix is to get a newer version of FFmpeg.
        if (codec->GetId() == mFFmpeg->GetAVCodecID(AUDACITY_AV_CODEC_ID_AAC)) {
            std::vector<int> rates;
            int i = 0;

            while (codec->GetSupportedSamplerates()[i]
                   && codec->GetSupportedSamplerates()[i] != 7350)
            {
                rates.push_back(codec->GetSupportedSamplerates()[i++]);
            }

            rates.push_back(0);

            if (!CheckSampleRate(mSampleRate, 0, 0, rates.data())) {
                mSampleRate = AskResample(0, mSampleRate, 0, 0, rates.data());
                mEncAudioCodecCtx->SetSampleRate(mSampleRate);
            }
        } else {
            if (!CheckSampleRate(
                    mSampleRate, 0, 0, codec->GetSupportedSamplerates())) {
                mSampleRate = AskResample(
                    0, mSampleRate, 0, 0, codec->GetSupportedSamplerates());
                mEncAudioCodecCtx->SetSampleRate(mSampleRate);
            }
        }

        // This happens if user refused to resample the project
        if (mSampleRate == 0) {
            return false;
        }
    }

    if (mEncFormatCtx->GetOutputFormat()->GetFlags() & AUDACITY_AVFMT_GLOBALHEADER) {
        mEncAudioCodecCtx->SetFlags(mEncAudioCodecCtx->GetFlags() | AUDACITY_AV_CODEC_FLAG_GLOBAL_HEADER);
        mEncFormatCtx->SetFlags(mEncFormatCtx->GetFlags() | AUDACITY_AV_CODEC_FLAG_GLOBAL_HEADER);
    }

    // Open the codec.
    int rc = mEncAudioCodecCtx->Open(codec.get(), &options);
    if (rc < 0) {
        TranslatableString errmsg;

        switch (rc) {
        case AUDACITY_AVERROR(EPERM):
            errmsg = XO("The codec reported a generic error (EPERM)");
            break;
        case AUDACITY_AVERROR(EINVAL):
            errmsg = XO("The codec reported an invalid parameter (EINVAL)");
            break;
        default:
            char buf[64];
            mFFmpeg->av_strerror(rc, buf, sizeof(buf));
            errmsg = Verbatim(buf);
        }

        /* i18n-hint: "codec" is short for a "coder-decoder" algorithm */
        throw ExportException(XO("Can't open audio codec \"%s\" (0x%x)\n\n%s")
                              .Format(codec->GetName(), codecID.value, errmsg)
                              .Translation());
    }

    mDefaultFrameSize = mEncAudioCodecCtx->GetFrameSize();

    if (mDefaultFrameSize == 0) {
        mDefaultFrameSize = 1024; // arbitrary non zero value;
    }
    wxLogDebug(
        wxT("FFmpeg : Audio Output Codec Frame Size: %d samples."),
        mEncAudioCodecCtx->GetFrameSize());

    // The encoder may require a minimum number of raw audio samples for each encoding but we can't
    // guarantee we'll get this minimum each time an audio frame is decoded from the input file so
    // we use a FIFO to store up incoming raw samples until we have enough for one call to the codec.
    mEncAudioFifo = std::make_unique<FifoBuffer>(mDefaultFrameSize * mChannels * sizeof(int16_t));

    mEncAudioFifoOutBufSize = 2 * MaxAudioPacketSize;
    // Allocate a buffer to read OUT of the FIFO into. The FIFO maintains its own buffer internally.
    mEncAudioFifoOutBuf = mFFmpeg->CreateMemoryBuffer<int16_t>(mEncAudioFifoOutBufSize);

    if (mEncAudioFifoOutBuf.empty()) {
        throw ExportException(_("FFmpeg : ERROR - Can't allocate buffer to read into from audio FIFO."));
    }

    return true;
}

void FFmpegExporter::WritePacket(AVPacketWrapper& pkt)
{
    // Set presentation time of frame (currently in the codec's timebase) in the
    // stream timebase.
    if (pkt.GetPresentationTimestamp() != AUDACITY_AV_NOPTS_VALUE) {
        pkt.RescalePresentationTimestamp(
            mEncAudioCodecCtx->GetTimeBase(), mEncAudioStream->GetTimeBase());
    }

    if (pkt.GetDecompressionTimestamp() != AUDACITY_AV_NOPTS_VALUE) {
        pkt.RescaleDecompressionTimestamp(
            mEncAudioCodecCtx->GetTimeBase(), mEncAudioStream->GetTimeBase());
    }

    if (pkt.GetDuration() > 0) {
        pkt.RescaleDuration(
            mEncAudioCodecCtx->GetTimeBase(), mEncAudioStream->GetTimeBase());
    }

    if (
        mFFmpeg->av_interleaved_write_frame(
            mEncFormatCtx->GetWrappedValue(), pkt.GetWrappedValue()) != 0) {
        throw ExportException(_("FFmpeg : ERROR - Couldn't write audio frame to output file."));
    }
}

// Returns 0 if no more output, 1 if more output, negative if error
int FFmpegExporter::EncodeAudio(AVPacketWrapper& pkt, int16_t* audio_samples, int nb_samples)
{
    // Assume *pkt is already initialized.

    int i, ch, buffer_size, ret, got_output = 0;
    AVDataBuffer<uint8_t> samples;

    std::unique_ptr<AVFrameWrapper> frame;

    if (audio_samples) {
        frame = mFFmpeg->CreateAVFrameWrapper();

        if (!frame) {
            return AUDACITY_AVERROR(ENOMEM);
        }

        frame->SetSamplesCount(nb_samples);
        frame->SetFormat(mEncAudioCodecCtx->GetSampleFmt());
        frame->SetChannelLayout(mEncAudioCodecCtx->GetChannelLayout());

        buffer_size = mFFmpeg->av_samples_get_buffer_size(
            NULL, mEncAudioCodecCtx->GetChannels(), nb_samples,
            mEncAudioCodecCtx->GetSampleFmt(), 0);

        if (buffer_size < 0) {
            throw ExportException(_("FFmpeg : ERROR - Could not get sample buffer size"));
        }

        samples = mFFmpeg->CreateMemoryBuffer<uint8_t>(buffer_size);

        if (samples.empty()) {
            throw ExportException(_("FFmpeg : ERROR - Could not allocate bytes for samples buffer"));
        }
        /* setup the data pointers in the AVFrame */
        ret = mFFmpeg->avcodec_fill_audio_frame(
            frame->GetWrappedValue(), mEncAudioCodecCtx->GetChannels(),
            mEncAudioCodecCtx->GetSampleFmt(), samples.data(), buffer_size, 0);

        if (ret < 0) {
            throw ExportException(_("FFmpeg : ERROR - Could not setup audio frame"));
        }

        const int channelsCount = mEncAudioCodecCtx->GetChannels();

        for (ch = 0; ch < mEncAudioCodecCtx->GetChannels(); ch++) {
            for (i = 0; i < nb_samples; i++) {
                switch (static_cast<AudacityAVSampleFormat>(
                            mEncAudioCodecCtx->GetSampleFmt())) {
                case AUDACITY_AV_SAMPLE_FMT_U8:
                    ((uint8_t*)(frame->GetData(0)))[ch + i * channelsCount] = audio_samples[ch + i * channelsCount] / 258 + 128;
                    break;
                case AUDACITY_AV_SAMPLE_FMT_U8P:
                    ((uint8_t*)(frame->GetData(ch)))[i] = audio_samples[ch + i * channelsCount] / 258 + 128;
                    break;
                case AUDACITY_AV_SAMPLE_FMT_S16:
                    ((int16_t*)(frame->GetData(0)))[ch + i * channelsCount] = audio_samples[ch + i * channelsCount];
                    break;
                case AUDACITY_AV_SAMPLE_FMT_S16P:
                    ((int16_t*)(frame->GetData(ch)))[i] = audio_samples[ch + i * channelsCount];
                    break;
                case AUDACITY_AV_SAMPLE_FMT_S32:
                    ((int32_t*)(frame->GetData(0)))[ch + i * channelsCount] = audio_samples[ch + i * channelsCount] << 16;
                    break;
                case AUDACITY_AV_SAMPLE_FMT_S32P:
                    ((int32_t*)(frame->GetData(ch)))[i] = audio_samples[ch + i * channelsCount] << 16;
                    break;
                case AUDACITY_AV_SAMPLE_FMT_FLT:
                    ((float*)(frame->GetData(0)))[ch + i * channelsCount] = audio_samples[ch + i * channelsCount] / 32767.0;
                    break;
                case AUDACITY_AV_SAMPLE_FMT_FLTP:
                    ((float*)(frame->GetData(ch)))[i] = audio_samples[ch + i * channelsCount] / 32767.;
                    break;
                default:
                    wxASSERT(false);
                    break;
                }
            }
        }
    }

    pkt.ResetData();

    pkt.SetStreamIndex(mEncAudioStream->GetIndex());

    if (mFFmpeg->avcodec_send_frame != nullptr) {
        ret = mFFmpeg->avcodec_send_frame(
            mEncAudioCodecCtx->GetWrappedValue(),
            frame ? frame->GetWrappedValue() : nullptr);

        while (ret >= 0)
        {
            ret = mFFmpeg->avcodec_receive_packet(
                mEncAudioCodecCtx->GetWrappedValue(), pkt.GetWrappedValue());

            if (ret == AUDACITY_AVERROR(EAGAIN) || ret == AUDACITY_AVERROR_EOF) {
                ret = 0;
                break;
            } else if (ret < 0) {
                break;
            }

            WritePacket(pkt);

            got_output = true;
        }
    } else {
        ret = mFFmpeg->avcodec_encode_audio2(
            mEncAudioCodecCtx->GetWrappedValue(), pkt.GetWrappedValue(),
            frame ? frame->GetWrappedValue() : nullptr, &got_output);

        if (ret == 0) {
            WritePacket(pkt);
        }
    }

    if (ret < 0 && ret != AUDACITY_AVERROR_EOF) {
        char buf[64];
        mFFmpeg->av_strerror(ret, buf, sizeof(buf));
        wxLogDebug(buf);

        throw ExportException(_("FFmpeg : ERROR - encoding frame failed"));
    }

    pkt.ResetTimestamps(); // We don't set frame timestamps thus don't trust the AVPacket timestamps

    return got_output;
}

bool FFmpegExporter::Finalize()
{
    // Flush the audio FIFO and encoder.
    for (;;) {
        std::unique_ptr<AVPacketWrapper> pkt = mFFmpeg->CreateAVPacketWrapper();

        const auto nFifoBytes
            =mEncAudioFifo->GetAvailable(); // any bytes left in audio FIFO?

        int encodeResult = 0;

        // Flush the audio FIFO first if necessary. It won't contain a _full_ audio frame because
        // if it did we'd have pulled it from the FIFO during the last encodeAudioFrame() call
        if (nFifoBytes > 0) {
            const int nAudioFrameSizeOut = mDefaultFrameSize * mEncAudioCodecCtx->GetChannels() * sizeof(int16_t);

            if (nAudioFrameSizeOut > mEncAudioFifoOutBufSize || nFifoBytes > mEncAudioFifoOutBufSize) {
                throw ExportException(_("FFmpeg : ERROR - Too much remaining data."));
            }

            // We have an incomplete buffer of samples left, encode it.
            // If codec supports CODEC_CAP_SMALL_LAST_FRAME, we can feed it with smaller frame
            // Or if frame_size is 1, then it's some kind of PCM codec, they don't have frames and will be fine with the samples
            // Otherwise we'll send a full frame of audio + silence padding to ensure all audio is encoded
            int frame_size = mDefaultFrameSize;
            if (
                mEncAudioCodecCtx->GetCodec()->GetCapabilities()
                & AUDACITY_AV_CODEC_CAP_SMALL_LAST_FRAME
                || frame_size == 1) {
                frame_size = nFifoBytes
                             / (mEncAudioCodecCtx->GetChannels() * sizeof(int16_t));
            }

            wxLogDebug(wxT("FFmpeg : Audio FIFO still contains %lld bytes, writing %d sample frame ..."),
                       nFifoBytes, frame_size);

            // Fill audio buffer with zeroes. If codec tries to read the whole buffer,
            // it will just read silence. If not - who cares?
            memset(mEncAudioFifoOutBuf.data(), 0, mEncAudioFifoOutBufSize);
            //const AVCodec *codec = mEncAudioCodecCtx->codec;

            // Pull the bytes out from the FIFO and feed them to the encoder.
            if (mEncAudioFifo->Read(mEncAudioFifoOutBuf.data(), nFifoBytes) == nFifoBytes) {
                encodeResult = EncodeAudio(*pkt, mEncAudioFifoOutBuf.data(), frame_size);
            } else {
                wxLogDebug(wxT("FFmpeg : Reading from Audio FIFO failed, aborting"));
                // TODO: more precise message
                throw ExportErrorException("FFmpeg:825");
            }
        } else {
            // Fifo is empty, flush encoder. May be called multiple times.
            encodeResult
                =EncodeAudio(*pkt.get(), nullptr, 0);
        }

        if (encodeResult < 0) {
            // TODO: more precise message
            throw ExportErrorException("FFmpeg:837");
        } else if (encodeResult == 0) {
            break;
        }
    }

    // Write any file trailers.
    if (mFFmpeg->av_write_trailer(mEncFormatCtx->GetWrappedValue()) != 0) {
        // TODO: more precise message
        throw ExportErrorException("FFmpeg:868");
    }

    return true;
}

// All paths in this that fail must report their error to the user.
bool FFmpegExporter::EncodeAudioFrame(int16_t* pFrame, size_t numSamples)
{
    const auto frameSize = numSamples * sizeof(int16_t) * mChannels;
    int nBytesToWrite = 0;
    uint8_t* pRawSamples = nullptr;
    int nAudioFrameSizeOut = mDefaultFrameSize * mEncAudioCodecCtx->GetChannels() * sizeof(int16_t);
    int ret;

    nBytesToWrite = frameSize;
    pRawSamples  = (uint8_t*)pFrame;

    // Put the raw audio samples into the FIFO.
    ret = mEncAudioFifo->Write(pRawSamples, nBytesToWrite);

    if (ret != nBytesToWrite) {
        throw ExportErrorException("FFmpeg:913");
    }

    if (nAudioFrameSizeOut > mEncAudioFifoOutBufSize) {
        throw ExportException(_("FFmpeg : ERROR - nAudioFrameSizeOut too large."));
    }

    // Read raw audio samples out of the FIFO in nAudioFrameSizeOut byte-sized groups to encode.
    while (mEncAudioFifo->GetAvailable() >= nAudioFrameSizeOut)
    {
        mEncAudioFifo->Read(
            mEncAudioFifoOutBuf.data(), nAudioFrameSizeOut);

        std::unique_ptr<AVPacketWrapper> pkt = mFFmpeg->CreateAVPacketWrapper();

        ret = EncodeAudio(*pkt,                     // out
                          mEncAudioFifoOutBuf.data(), // in
                          mDefaultFrameSize);

        if (ret < 0) {
            return false;
        }
    }
    return true;
}

FFmpegExportProcessor::FFmpegExportProcessor(std::shared_ptr<FFmpegFunctions> ffmpeg, int subformat)
    : mFFmpeg(std::move(ffmpeg))
{
    context.subformat = subformat;
}

bool FFmpegExportProcessor::Initialize(AudacityProject& project,
                                       const Parameters& parameters,
                                       const wxFileNameWrapper& fName,
                                       double t0, double t1, bool selectionOnly,
                                       double sampleRate, unsigned channels,
                                       MixerOptions::Downmix* mixerSpec,
                                       const Tags* metadata)
{
    context.t0 = t0;
    context.t1 = t1;

    if (!FFmpegFunctions::Load()) {
        throw ExportException(_("Properly configured FFmpeg is required to proceed.\nYou can configure it at Preferences > Libraries."));
    }
    // subformat index may not correspond directly to fmts[] index, convert it
    const auto adjustedFormatIndex = AdjustFormatIndex(context.subformat);
    if (channels > ExportFFmpegOptions::fmts[adjustedFormatIndex].maxchannels) {
        throw ExportException(XO("Attempted to export %d channels, but maximum number of channels for selected output format is %d")
                              .Format(
                                  channels,
                                  ExportFFmpegOptions::fmts[adjustedFormatIndex].maxchannels)
                              .Translation());
    }

    bool ret = true;

    if (adjustedFormatIndex >= FMT_LAST) {
        // TODO: more precise message
        throw ExportErrorException("FFmpeg:996");
    }

    wxString shortname(ExportFFmpegOptions::fmts[adjustedFormatIndex].shortname);
    if (adjustedFormatIndex == FMT_OTHER) {
        shortname = ExportPluginHelpers::GetParameterValue<std::string>(parameters, FEFormatID, "matroska");
    }

    context.exporter = std::make_unique<FFmpegExporter>(mFFmpeg, fName, channels, adjustedFormatIndex);

    ret = context.exporter->Init(shortname.mb_str(), &project, static_cast<int>(sampleRate), metadata, parameters);

    if (!ret) {
        // TODO: more precise message
        throw ExportErrorException("FFmpeg:1008");
    }

    context.mixer
        =context.exporter->CreateMixer(project, selectionOnly, t0, t1, mixerSpec);

    context.status = selectionOnly
                     ? XO("Exporting selected audio as %s")
                     .Format(ExportFFmpegOptions::fmts[adjustedFormatIndex].description)
                     : XO("Exporting the audio as %s")
                     .Format(ExportFFmpegOptions::fmts[adjustedFormatIndex].description);

    return true;
}

ExportResult FFmpegExportProcessor::Process(ExportProcessorDelegate& delegate)
{
    delegate.SetStatusString(context.status);
    auto exportResult = ExportResult::Success;
    {
        while (exportResult == ExportResult::Success) {
            auto pcmNumSamples = context.mixer->Process();
            if (pcmNumSamples == 0) {
                break;
            }

            short* pcmBuffer = (short*)context.mixer->GetBuffer();

            if (!context.exporter->EncodeAudioFrame(pcmBuffer, pcmNumSamples)) {
                // All errors should already have been reported.
                return ExportResult::Error;
            }

            if (exportResult == ExportResult::Success) {
                exportResult = ExportPluginHelpers::UpdateProgress(
                    delegate, *context.mixer, context.t0, context.t1);
            }
        }
    }

    if (exportResult != ExportResult::Cancelled) {
        if (!context.exporter->Finalize()) { // Finalize makes its own messages
            return ExportResult::Error;
        }
    }
    return exportResult;
}

void AddStringTagUTF8(char field[], int size, wxString value)
{
    memset(field, 0, size);
    memcpy(field, value.ToUTF8(), (int)strlen(value.ToUTF8()) > size - 1 ? size - 1 : strlen(value.ToUTF8()));
}

void AddStringTagANSI(char field[], int size, wxString value)
{
    memset(field, 0, size);
    memcpy(field, value.mb_str(), (int)strlen(value.mb_str()) > size - 1 ? size - 1 : strlen(value.mb_str()));
}

bool FFmpegExporter::AddTags(const Tags* tags)
{
    if (tags == NULL) {
        return false;
    }

    SetMetadata(tags, "album", TAG_ALBUM);
    SetMetadata(tags, "comment", TAG_COMMENTS);
    SetMetadata(tags, "genre", TAG_GENRE);
    SetMetadata(tags, "title", TAG_TITLE);
    SetMetadata(tags, "track", TAG_TRACK);

    // Bug 2564: Add m4a tags
    if (mEncFormatDesc->GetAudioCodec() == mFFmpeg->GetAVCodecID(AUDACITY_AV_CODEC_ID_AAC)) {
        SetMetadata(tags, "artist", TAG_ARTIST);
        SetMetadata(tags, "date", TAG_YEAR);
    } else {
        SetMetadata(tags, "author", TAG_ARTIST);
        SetMetadata(tags, "year", TAG_YEAR);
    }

    return true;
}

void FFmpegExporter::SetMetadata(const Tags* tags, const char* name, const wxChar* tag)
{
    if (tags->HasTag(tag)) {
        wxString value = tags->GetTag(tag);

        AVDictionaryWrapper metadata = mEncFormatCtx->GetMetadata();

        metadata.Set(name, mSupportsUTF8 ? value : value.mb_str(), 0);
        mEncFormatCtx->SetMetadata(metadata);
    }
}

//----------------------------------------------------------------------------
// AskResample dialog
//----------------------------------------------------------------------------

int FFmpegExporter::AskResample(int bitrate, int rate, int lowrate, int highrate, const int* sampRates)
{
#if defined(FFMPEG_AUTO_RESAMPLE)
    std::vector<int> rates;

    for (int i = 0; sampRates[i]; ++i) {
        rates.push_back(sampRates[i]);
    }

    std::sort(rates.begin(), rates.end());

    int bestRate = 0;
    for (auto i : rates) {
        bestRate = i;
        if (i > rate) {
            break;
        }
    }

    return bestRate;
#else
    wxDialogWrapper d(nullptr, wxID_ANY, XO("Invalid sample rate"));
    d.SetName();
    wxChoice* choice;
    ShuttleGui S(&d, eIsCreating);

    int selected = -1;

    S.StartVerticalLay();
    {
        S.SetBorder(10);
        S.StartStatic(XO("Resample"));
        {
            S.StartHorizontalLay(wxALIGN_CENTER, false);
            {
                S.AddTitle(
                    (bitrate == 0
                     ? XO(
                         "The project sample rate (%d) is not supported by the current output\nfile format. ")
                     .Format(rate)
                     : XO(
                         "The project sample rate (%d) and bit rate (%d kbps) combination is not\nsupported by the current output file format. ")
                     .Format(rate, bitrate / 1000))
                    + XO("You may resample to one of the rates below.")
                    );
            }
            S.EndHorizontalLay();

            S.StartHorizontalLay(wxALIGN_CENTER, false);
            {
                choice = S.AddChoice(XO("Sample Rates"),
                                     [&]{
                    TranslatableStrings choices;
                    for (int i = 0; sampRates[i] > 0; i++) {
                        int label = sampRates[i];
                        if ((!lowrate || label >= lowrate) && (!highrate || label <= highrate)) {
                            wxString name = wxString::Format(wxT("%d"), label);
                            choices.push_back(Verbatim(name));
                            if (label <= rate) {
                                selected = i;
                            }
                        }
                    }
                    return choices;
                }(),
                                     std::max(0, selected)
                                     );
            }
            S.EndHorizontalLay();
        }
        S.EndStatic();

        S.AddStandardButtons();
    }
    S.EndVerticalLay();

    d.Layout();
    d.Fit();
    d.SetMinSize(d.GetSize());
    d.Center();

    if (d.ShowModal() == wxID_CANCEL) {
        return 0;
    }

    return wxAtoi(choice->GetStringSelection());
#endif
}

static ExportPluginRegistry::RegisteredPlugin sRegisteredPlugin{ "FFmpeg",
                                                                 []{ return std::make_unique< ExportFFmpeg >(); }
};
