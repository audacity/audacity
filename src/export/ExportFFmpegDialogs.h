/**********************************************************************

Audacity: A Digital Audio Editor

ExportFFmpegDialogs.h

LRN

**********************************************************************/

#if !defined(__EXPORT_FFMPEG_DIALOGS_H__)
#define __EXPORT_FFMPEG_DIALOGS_H__

#if defined(USE_FFMPEG)

#include "../Audacity.h"   // keep ffmpeg before wx because they interact
#include "../FFmpeg.h"     // and Audacity.h before FFmpeg for config*.h

#include <wx/listimpl.cpp>
#include "../xml/XMLFileReader.h"
#include "../FileNames.h"


/// Identifiers for pre-set export types.
enum FFmpegExposedFormat 
{
   FMT_M4A,
   FMT_AC3,
   FMT_AMRNB,
#if FFMPEG_STABLE
   FMT_AMRWB,
#endif
   FMT_WMA2,
   FMT_OTHER,
   FMT_LAST
};

/// Describes export type
struct ExposedFormat
{
   FFmpegExposedFormat fmtid; //!< one of the FFmpegExposedFormat
   const wxChar *name;        //!< format name (internal, should be unique; if not - export dialog may show unusual behaviour)
   const wxChar *extension;   //!< default extension for this format. More extensions may be added later via AddExtension.
   const wxChar *shortname;   //!< used to guess the format
   int maxchannels;           //!< how much channels this format could handle
   bool canmetadata;          //!< true if format supports metadata, false otherwise
   bool canutf8;              //!< true if format supports metadata in UTF-8, false otherwise
   const wxChar *description; //!< format description (will be shown in export dialog)
   CodecID codecid;           //!< codec ID (see libavcodec/avcodec.h)
   bool compiledIn;           //!< support for this codec/format is compiled in (checked at runtime)
};

/// List of export types
static ExposedFormat fmts[] = 
{
   {FMT_M4A,         wxT("M4A"),     wxT("m4a"),  wxT("ipod"), 48,  true ,true ,_("M4A (AAC) Files (FFmpeg)"),         CODEC_ID_AAC,    true},
   {FMT_AC3,         wxT("AC3"),     wxT("ac3"),  wxT("ac3"),  7,   false,false,_("AC3 Files (FFmpeg)"),               CODEC_ID_AC3,    true},
   {FMT_AMRNB,       wxT("AMRNB"),   wxT("amr"),  wxT("amr"),  1,   false,false,_("AMR (narrow band) Files (FFmpeg)"), CODEC_ID_AMR_NB, true},
#if FFMPEG_STABLE
   {FMT_AMRWB,       wxT("AMRWB"),   wxT("amr"),  wxT("amr"),  1,   false,false,_("AMR (wide band) Files (FFmpeg)"),   CODEC_ID_AMR_WB, true},
#endif
   {FMT_WMA2,        wxT("WMA"),     wxT("wma"),  wxT("asf"),  2,   false,false,_("WMA (version 2) Files (FFmpeg)"),   CODEC_ID_WMAV2,  true},
   {FMT_OTHER,       wxT("FFMPEG"),  wxT(""),     wxT(""),     255, true ,true ,_("Custom FFmpeg Export"),             CODEC_ID_NONE,   true}
};

/// Describes format-codec compatibility
struct CompatibilityEntry
{
   const wxChar *fmt; //!< format, recognizeable by guess_format()
   CodecID codec;     //!< codec ID
};

/// Format-codec compatibility list
/// Must end with NULL entry
static CompatibilityEntry CompatibilityList[] = 
{
   { wxT("adts"), CODEC_ID_AAC },

   { wxT("aiff"), CODEC_ID_PCM_S16BE },
   { wxT("aiff"), CODEC_ID_PCM_S8 },
   { wxT("aiff"), CODEC_ID_PCM_S24BE },
   { wxT("aiff"), CODEC_ID_PCM_S32BE },
   { wxT("aiff"), CODEC_ID_PCM_ALAW },
   { wxT("aiff"), CODEC_ID_PCM_MULAW },
   { wxT("aiff"), CODEC_ID_MACE3 },
   { wxT("aiff"), CODEC_ID_MACE6 },
   { wxT("aiff"), CODEC_ID_GSM },
   { wxT("aiff"), CODEC_ID_ADPCM_G726 },
   { wxT("aiff"), CODEC_ID_PCM_S16LE },
   { wxT("aiff"), CODEC_ID_ADPCM_IMA_QT },
   { wxT("aiff"), CODEC_ID_QDM2 },

   { wxT("amr"), CODEC_ID_AMR_NB },
   { wxT("amr"), CODEC_ID_AMR_WB },

   { wxT("asf"), CODEC_ID_PCM_S16LE },
   { wxT("asf"), CODEC_ID_PCM_U8 },
   { wxT("asf"), CODEC_ID_PCM_S24LE },
   { wxT("asf"), CODEC_ID_PCM_S32LE },
   { wxT("asf"), CODEC_ID_ADPCM_MS },
   { wxT("asf"), CODEC_ID_PCM_ALAW },
   { wxT("asf"), CODEC_ID_PCM_MULAW },
   { wxT("asf"), CODEC_ID_WMAVOICE },
   { wxT("asf"), CODEC_ID_ADPCM_IMA_WAV },
   { wxT("asf"), CODEC_ID_ADPCM_YAMAHA },
   { wxT("asf"), CODEC_ID_TRUESPEECH },
   { wxT("asf"), CODEC_ID_GSM_MS },
   { wxT("asf"), CODEC_ID_ADPCM_G726 },
   { wxT("asf"), CODEC_ID_MP2 },
   { wxT("asf"), CODEC_ID_MP3 },
   { wxT("asf"), CODEC_ID_VOXWARE },
   { wxT("asf"), CODEC_ID_AAC },
   { wxT("asf"), CODEC_ID_WMAV1 },
   { wxT("asf"), CODEC_ID_WMAV2 },
   { wxT("asf"), CODEC_ID_WMAPRO },
   { wxT("asf"), CODEC_ID_ADPCM_CT },
   { wxT("asf"), CODEC_ID_ATRAC3 },
   { wxT("asf"), CODEC_ID_IMC },
   { wxT("asf"), CODEC_ID_AC3 },
   { wxT("asf"), CODEC_ID_DTS },
   { wxT("asf"), CODEC_ID_SONIC },
   { wxT("asf"), CODEC_ID_SONIC_LS },
   { wxT("asf"), CODEC_ID_FLAC },
   { wxT("asf"), CODEC_ID_ADPCM_SWF },
   { wxT("asf"), CODEC_ID_VORBIS },

   { wxT("au"), CODEC_ID_PCM_MULAW },
   { wxT("au"), CODEC_ID_PCM_S8 },
   { wxT("au"), CODEC_ID_PCM_S16BE },
   { wxT("au"), CODEC_ID_PCM_ALAW },

   { wxT("avi"), CODEC_ID_PCM_S16LE },
   { wxT("avi"), CODEC_ID_PCM_U8 },
   { wxT("avi"), CODEC_ID_PCM_S24LE },
   { wxT("avi"), CODEC_ID_PCM_S32LE },
   { wxT("avi"), CODEC_ID_ADPCM_MS },
   { wxT("avi"), CODEC_ID_PCM_ALAW },
   { wxT("avi"), CODEC_ID_PCM_MULAW },
   { wxT("avi"), CODEC_ID_WMAVOICE },
   { wxT("avi"), CODEC_ID_ADPCM_IMA_WAV },
   { wxT("avi"), CODEC_ID_ADPCM_YAMAHA },
   { wxT("avi"), CODEC_ID_TRUESPEECH },
   { wxT("avi"), CODEC_ID_GSM_MS },
   { wxT("avi"), CODEC_ID_ADPCM_G726 },
   { wxT("avi"), CODEC_ID_MP2 },
   { wxT("avi"), CODEC_ID_MP3 },
   { wxT("avi"), CODEC_ID_VOXWARE },
   { wxT("avi"), CODEC_ID_AAC },
   { wxT("avi"), CODEC_ID_WMAV1 },
   { wxT("avi"), CODEC_ID_WMAV2 },
   { wxT("avi"), CODEC_ID_WMAPRO },
   { wxT("avi"), CODEC_ID_ADPCM_CT },
   { wxT("avi"), CODEC_ID_ATRAC3 },
   { wxT("avi"), CODEC_ID_IMC },
   { wxT("avi"), CODEC_ID_AC3 },
   { wxT("avi"), CODEC_ID_DTS },
   { wxT("avi"), CODEC_ID_SONIC },
   { wxT("avi"), CODEC_ID_SONIC_LS },
   { wxT("avi"), CODEC_ID_FLAC },
   { wxT("avi"), CODEC_ID_ADPCM_SWF },
   { wxT("avi"), CODEC_ID_VORBIS },

   { wxT("crc"), CODEC_ID_NONE },

   { wxT("dv"), CODEC_ID_PCM_S16LE },

   { wxT("ffm"), CODEC_ID_NONE },

   { wxT("flv"), CODEC_ID_MP3 },
   { wxT("flv"), CODEC_ID_PCM_S8 },
   { wxT("flv"), CODEC_ID_PCM_S16BE },
   { wxT("flv"), CODEC_ID_PCM_S16LE },
   { wxT("flv"), CODEC_ID_ADPCM_SWF },
   { wxT("flv"), CODEC_ID_AAC },
   { wxT("flv"), CODEC_ID_NELLYMOSER },

   { wxT("framecrc"), CODEC_ID_NONE },

   { wxT("gxf"), CODEC_ID_PCM_S16LE },

   { wxT("matroska"), CODEC_ID_PCM_S16LE },
   { wxT("matroska"), CODEC_ID_PCM_U8 },
   { wxT("matroska"), CODEC_ID_PCM_S24LE },
   { wxT("matroska"), CODEC_ID_PCM_S32LE },
   { wxT("matroska"), CODEC_ID_ADPCM_MS },
   { wxT("matroska"), CODEC_ID_PCM_ALAW },
   { wxT("matroska"), CODEC_ID_PCM_MULAW },
   { wxT("matroska"), CODEC_ID_WMAVOICE },
   { wxT("matroska"), CODEC_ID_ADPCM_IMA_WAV },
   { wxT("matroska"), CODEC_ID_ADPCM_YAMAHA },
   { wxT("matroska"), CODEC_ID_TRUESPEECH },
   { wxT("matroska"), CODEC_ID_GSM_MS },
   { wxT("matroska"), CODEC_ID_ADPCM_G726 },
   { wxT("matroska"), CODEC_ID_MP2 },
   { wxT("matroska"), CODEC_ID_MP3 },
   { wxT("matroska"), CODEC_ID_VOXWARE },
   { wxT("matroska"), CODEC_ID_AAC },
   { wxT("matroska"), CODEC_ID_WMAV1 },
   { wxT("matroska"), CODEC_ID_WMAV2 },
   { wxT("matroska"), CODEC_ID_WMAPRO },
   { wxT("matroska"), CODEC_ID_ADPCM_CT },
   { wxT("matroska"), CODEC_ID_ATRAC3 },
   { wxT("matroska"), CODEC_ID_IMC },
   { wxT("matroska"), CODEC_ID_AC3 },
   { wxT("matroska"), CODEC_ID_DTS },
   { wxT("matroska"), CODEC_ID_SONIC },
   { wxT("matroska"), CODEC_ID_SONIC_LS },
   { wxT("matroska"), CODEC_ID_FLAC },
   { wxT("matroska"), CODEC_ID_ADPCM_SWF },
   { wxT("matroska"), CODEC_ID_VORBIS },

   { wxT("mmf"), CODEC_ID_ADPCM_YAMAHA },

   { wxT("mov"), CODEC_ID_PCM_S32BE }, //mov
   { wxT("mov"), CODEC_ID_PCM_S32LE },
   { wxT("mov"), CODEC_ID_PCM_S24BE },
   { wxT("mov"), CODEC_ID_PCM_S24LE },
   { wxT("mov"), CODEC_ID_PCM_S16BE },
   { wxT("mov"), CODEC_ID_PCM_S16LE },
   { wxT("mov"), CODEC_ID_PCM_S8 },
   { wxT("mov"), CODEC_ID_PCM_U8 },
   { wxT("mov"), CODEC_ID_PCM_MULAW },
   { wxT("mov"), CODEC_ID_PCM_ALAW },
   { wxT("mov"), CODEC_ID_ADPCM_IMA_QT },
   { wxT("mov"), CODEC_ID_MACE3 },
   { wxT("mov"), CODEC_ID_MACE6 },
   { wxT("mov"), CODEC_ID_MP3 },
   { wxT("mov"), CODEC_ID_AAC },
   { wxT("mov"), CODEC_ID_AMR_NB },
   { wxT("mov"), CODEC_ID_AMR_WB },
   { wxT("mov"), CODEC_ID_GSM },
   { wxT("mov"), CODEC_ID_ALAC },
   { wxT("mov"), CODEC_ID_QCELP },
   { wxT("mov"), CODEC_ID_QDM2 },
   { wxT("mov"), CODEC_ID_DVAUDIO },
   { wxT("mov"), CODEC_ID_WMAV2 },
   { wxT("mov"), CODEC_ID_ALAC },

   { wxT("mp4"), CODEC_ID_AAC },
   { wxT("mp4"), CODEC_ID_QCELP },
   { wxT("mp4"), CODEC_ID_MP3 },
   { wxT("mp4"), CODEC_ID_VORBIS },

   { wxT("psp"), CODEC_ID_AAC },
   { wxT("psp"), CODEC_ID_QCELP },
   { wxT("psp"), CODEC_ID_MP3 },
   { wxT("psp"), CODEC_ID_VORBIS },

   { wxT("ipod"), CODEC_ID_AAC },
   { wxT("ipod"), CODEC_ID_QCELP },
   { wxT("ipod"), CODEC_ID_MP3 },
   { wxT("ipod"), CODEC_ID_VORBIS },

   { wxT("3gp"), CODEC_ID_AAC },
   { wxT("3gp"), CODEC_ID_AMR_NB },
   { wxT("3gp"), CODEC_ID_AMR_WB },

   { wxT("3g2"), CODEC_ID_AAC },
   { wxT("3g2"), CODEC_ID_AMR_NB },
   { wxT("3g2"), CODEC_ID_AMR_WB },

   { wxT("mp3"), CODEC_ID_MP3 },

   { wxT("mpeg"), CODEC_ID_AC3 },
   { wxT("mpeg"), CODEC_ID_DTS },
   { wxT("mpeg"), CODEC_ID_PCM_S16BE },
   { wxT("mpeg"), CODEC_ID_MP2 },

   { wxT("vcd"), CODEC_ID_AC3 },
   { wxT("vcd"), CODEC_ID_DTS },
   { wxT("vcd"), CODEC_ID_PCM_S16BE },
   { wxT("vcd"), CODEC_ID_MP2 },

   { wxT("vob"), CODEC_ID_AC3 },
   { wxT("vob"), CODEC_ID_DTS },
   { wxT("vob"), CODEC_ID_PCM_S16BE },
   { wxT("vob"), CODEC_ID_MP2 },

   { wxT("svcd"), CODEC_ID_AC3 },
   { wxT("svcd"), CODEC_ID_DTS },
   { wxT("svcd"), CODEC_ID_PCM_S16BE },
   { wxT("svcd"), CODEC_ID_MP2 },

   { wxT("dvd"), CODEC_ID_AC3 },
   { wxT("dvd"), CODEC_ID_DTS },
   { wxT("dvd"), CODEC_ID_PCM_S16BE },
   { wxT("dvd"), CODEC_ID_MP2 },

   { wxT("nut"), CODEC_ID_PCM_S16LE },
   { wxT("nut"), CODEC_ID_PCM_U8 },
   { wxT("nut"), CODEC_ID_PCM_S24LE },
   { wxT("nut"), CODEC_ID_PCM_S32LE },
   { wxT("nut"), CODEC_ID_ADPCM_MS },
   { wxT("nut"), CODEC_ID_PCM_ALAW },
   { wxT("nut"), CODEC_ID_PCM_MULAW },
   { wxT("nut"), CODEC_ID_WMAVOICE },
   { wxT("nut"), CODEC_ID_ADPCM_IMA_WAV },
   { wxT("nut"), CODEC_ID_ADPCM_YAMAHA },
   { wxT("nut"), CODEC_ID_TRUESPEECH },
   { wxT("nut"), CODEC_ID_GSM_MS },
   { wxT("nut"), CODEC_ID_ADPCM_G726 },
   { wxT("nut"), CODEC_ID_MP2 },
   { wxT("nut"), CODEC_ID_MP3 },
   { wxT("nut"), CODEC_ID_VOXWARE },
   { wxT("nut"), CODEC_ID_AAC },
   { wxT("nut"), CODEC_ID_WMAV1 },
   { wxT("nut"), CODEC_ID_WMAV2 },
   { wxT("nut"), CODEC_ID_WMAPRO },
   { wxT("nut"), CODEC_ID_ADPCM_CT },
   { wxT("nut"), CODEC_ID_ATRAC3 },
   { wxT("nut"), CODEC_ID_IMC },
   { wxT("nut"), CODEC_ID_AC3 },
   { wxT("nut"), CODEC_ID_DTS },
   { wxT("nut"), CODEC_ID_SONIC },
   { wxT("nut"), CODEC_ID_SONIC_LS },
   { wxT("nut"), CODEC_ID_FLAC },
   { wxT("nut"), CODEC_ID_ADPCM_SWF },
   { wxT("nut"), CODEC_ID_VORBIS },

   { wxT("ogg"), CODEC_ID_VORBIS },
   { wxT("ogg"), CODEC_ID_FLAC },

   { wxT("ac3"), CODEC_ID_AC3 },

   { wxT("dts"), CODEC_ID_DTS },

   { wxT("flac"), CODEC_ID_FLAC },

   { wxT("RoQ"), CODEC_ID_ROQ_DPCM },

   { wxT("rm"), CODEC_ID_AC3 },

   { wxT("swf"), CODEC_ID_MP3 },

   { wxT("avm2"), CODEC_ID_MP3 },

   { wxT("voc"), CODEC_ID_PCM_U8 },

   { wxT("wav"), CODEC_ID_PCM_S16LE },
   { wxT("wav"), CODEC_ID_PCM_U8 },
   { wxT("wav"), CODEC_ID_PCM_S24LE },
   { wxT("wav"), CODEC_ID_PCM_S32LE },
   { wxT("wav"), CODEC_ID_ADPCM_MS },
   { wxT("wav"), CODEC_ID_PCM_ALAW },
   { wxT("wav"), CODEC_ID_PCM_MULAW },
   { wxT("wav"), CODEC_ID_WMAVOICE },
   { wxT("wav"), CODEC_ID_ADPCM_IMA_WAV },
   { wxT("wav"), CODEC_ID_ADPCM_YAMAHA },
   { wxT("wav"), CODEC_ID_TRUESPEECH },
   { wxT("wav"), CODEC_ID_GSM_MS },
   { wxT("wav"), CODEC_ID_ADPCM_G726 },
   { wxT("wav"), CODEC_ID_MP2 },
   { wxT("wav"), CODEC_ID_MP3 },
   { wxT("wav"), CODEC_ID_VOXWARE },
   { wxT("wav"), CODEC_ID_AAC },
   { wxT("wav"), CODEC_ID_WMAV1 },
   { wxT("wav"), CODEC_ID_WMAV2 },
   { wxT("wav"), CODEC_ID_WMAPRO },
   { wxT("wav"), CODEC_ID_ADPCM_CT },
   { wxT("wav"), CODEC_ID_ATRAC3 },
   { wxT("wav"), CODEC_ID_IMC },
   { wxT("wav"), CODEC_ID_AC3 },
   { wxT("wav"), CODEC_ID_DTS },
   { wxT("wav"), CODEC_ID_SONIC },
   { wxT("wav"), CODEC_ID_SONIC_LS },
   { wxT("wav"), CODEC_ID_FLAC },
   { wxT("wav"), CODEC_ID_ADPCM_SWF },
   { wxT("wav"), CODEC_ID_VORBIS },

   { NULL, CODEC_ID_NONE }
};


/// AAC profiles
static int iAACProfileValues[] = { FF_PROFILE_AAC_LOW, FF_PROFILE_AAC_MAIN, /*FF_PROFILE_AAC_SSR,*/ FF_PROFILE_AAC_LTP };

/// Names of AAC profiles to be displayed
static const wxChar *iAACProfileNames[] = { _("LC"), _("Main"), /*_("SSR"),*/ _("LTP") }; //SSR is not supported

/// Sample rates supported by AAC encoder (must end with zero-element)
static const int iAACSampleRates[] = { 7350, 8000, 11025, 12000, 16000, 22050, 24000, 32000, 44100, 38000, 64000, 88200, 0 };

/// Bit Rates supported by libAMR-NB encoder
/// Sample Rate is always 8 kHz
static int iAMRNBBitRate[] = { 4750, 5150, 5900, 6700, 7400, 7950, 10200, 12200 };

/// Bit Rates supported by libAMR-WB encoder
/// Sample Rate is always 16 kHz
static int iAMRWBBitRate[] = { 6600, 8850, 12650, 14250, 15850, 18250, 19850, 23050, 23850 };

/// Bit Rates supported by WMA encoder. Setting bit rate to other values will not result in different file size.
static int iWMABitRate[] = { 24634, 26012, 27734, 29457, 31524, 33764, 36348, 39448, 42894, 47028, 52024, 58225, 65805, 75624, 88716, 106976, 134539, 180189, 271835, 546598 };

/// AC3 export options dialog
class ExportFFmpegAC3Options : public wxDialog
{
public:

   ExportFFmpegAC3Options(wxWindow *parent);
   void PopulateOrExchange(ShuttleGui & S);
   void OnOK(wxCommandEvent& event);
   /// Bit Rates supported by AC3 encoder
   static const int iAC3BitRates[];
   /// Sample Rates supported by AC3 encoder (must end with zero-element)
   /// It is not used in dialog anymore, but will be required later
   static const int iAC3SampleRates[];

private:

   wxArrayString mBitRateNames;
   wxArrayInt    mBitRateLabels;

   wxChoice *mBitRateChoice;
   wxButton *mOk;
   int mBitRateFromChoice;

   DECLARE_EVENT_TABLE()
};

class ExportFFmpegAACOptions : public wxDialog
{
public:

   ExportFFmpegAACOptions(wxWindow *parent);
   void PopulateOrExchange(ShuttleGui & S);
   void OnOK(wxCommandEvent& event);

private:

   wxSpinCtrl *mQualitySpin;
   wxButton *mOk;

   DECLARE_EVENT_TABLE()
};

class ExportFFmpegAMRNBOptions : public wxDialog
{
public:

   ExportFFmpegAMRNBOptions(wxWindow *parent);
   void PopulateOrExchange(ShuttleGui & S);
   void OnOK(wxCommandEvent& event);

private:

   wxArrayString mBitRateNames;
   wxArrayInt    mBitRateLabels;

   wxChoice *mBitRateChoice;
   wxButton *mOk;
   int mBitRateFromChoice;

   DECLARE_EVENT_TABLE()
};

class ExportFFmpegAMRWBOptions : public wxDialog
{
public:

   ExportFFmpegAMRWBOptions(wxWindow *parent);
   void PopulateOrExchange(ShuttleGui & S);
   void OnOK(wxCommandEvent& event);

private:

   wxArrayString mBitRateNames;
   wxArrayInt    mBitRateLabels;

   wxChoice *mBitRateChoice;
   wxButton *mOk;
   int mBitRateFromChoice;

   DECLARE_EVENT_TABLE()
};

class ExportFFmpegWMAOptions : public wxDialog
{
public:

   ExportFFmpegWMAOptions(wxWindow *parent);
   void PopulateOrExchange(ShuttleGui & S);
   void OnOK(wxCommandEvent& event);

   static const int iWMASampleRates[];

private:

   wxArrayString mBitRateNames;
   wxArrayInt    mBitRateLabels;

   wxChoice *mBitRateChoice;
   wxButton *mOk;
   int mBitRateFromChoice;

   DECLARE_EVENT_TABLE()
};

/// Identifiers for UI elements of the Custom FFmpeg export dialog
/// Do not store these in external files, as they may be changed later
enum FFmpegExportCtrlID {
   FEFirstID = 20000,
   FEFormatID,
   FECodecID,
   FEBitrateID,
   FEQualityID,
   FESampleRateID,
   FELanguageID,
   FETagID,
   FECutoffID,
   FEFrameSizeID,
   FEBufSizeID,
   FEProfileID,
   FECompLevelID,
   FEUseLPCID,
   FELPCCoeffsID,
   FEMinPredID,
   FEMaxPredID,
   FEPredOrderID,
   FEMinPartOrderID,
   FEMaxPartOrderID,
   FEMuxRateID,
   FEPacketSizeID,
   FEBitReservoirID,
   FEVariableBlockLenID,
   FELastID,

   FEFormatLabelID,
   FECodecLabelID,
   FEFormatNameID,
   FECodecNameID,
   FEPresetID,
   FESavePresetID,
   FELoadPresetID,
   FEDeletePresetID,
   FEAllFormatsID,
   FEAllCodecsID,
   FEImportPresetsID,
   FEExportPresetsID
};

/// String equivalents of identifiers for UI elements
/// These may be stored in external files
/// I have not yet found a convinient way to keep these two lists in sync automatically
/// To get control's ID string, use FFmpegExportCtrlIDNames[ControlID - FEFirstID]
static const wxChar *FFmpegExportCtrlIDNames[] = {
   wxT("FEFirstID"),
   wxT("FEFormatID"),
   wxT("FECodecID"),
   wxT("FEBitrateID"),
   wxT("FEQualityID"),
   wxT("FESampleRateID"),
   wxT("FELanguageID"),
   wxT("FETagID"),
   wxT("FECutoffID"),
   wxT("FEFrameSizeID"),
   wxT("FEBufSizeID"),
   wxT("FEProfileID"),
   wxT("FECompLevelID"),
   wxT("FEUseLPCID"),
   wxT("FELPCCoeffsID"),
   wxT("FEMinPredID"),
   wxT("FEMaxPredID"),
   wxT("FEPredOrderID"),
   wxT("FEMinPartOrderID"),
   wxT("FEMaxPartOrderID"),
   wxT("FEMuxRateID"),
   wxT("FEPacketSizeID"),
   wxT("FEBitReservoirID"),
   wxT("FEVariableBlockLenID"),
   wxT("FELastID"),

   wxT("FEFormatLabelID"),
   wxT("FECodecLabelID"),
   wxT("FEFormatNameID"),
   wxT("FECodecNameID"),
   wxT("FEPresetID"),
   wxT("FESavePresetID"),
   wxT("FELoadPresetID"),
   wxT("FEDeletePresetID"),
   wxT("FEAllFormatsID"),
   wxT("FEAllCodecsID"),
   wxT("FEImportPresetsID"),
   wxT("FEExportPresetsID")
};

/// Entry for the Applicability table
struct ApplicableFor
{
   bool                 enable;  //!< true if this control should be enabled, false otherwise
   FFmpegExportCtrlID   control; //!< control ID
   CodecID              codec;   //!< Codec ID
   const char          *format;  //!< Format short name
};

/// Some controls (parameters they represent) are only applicable to a number
/// of codecs and/or formats.
/// Syntax: first, enable a control for each applicable format-codec combination
/// then disable it for anything else
/// "any" - any format
/// CODEC_ID_NONE - any codec
/// This list must end with {FALSE,FFmpegExportCtrlID(0),CODEC_ID_NONE,NULL}
static ApplicableFor apptable[] = 
{
   {TRUE,FEQualityID,CODEC_ID_AAC,"any"},
   {TRUE,FEQualityID,CODEC_ID_MP3,"any"},
   {TRUE,FEQualityID,CODEC_ID_VORBIS,"any"},
   {FALSE,FEQualityID,CODEC_ID_NONE,"any"},

   {TRUE,FECutoffID,CODEC_ID_AC3,"any"},
   {TRUE,FECutoffID,CODEC_ID_AAC,"any"},
   {TRUE,FECutoffID,CODEC_ID_VORBIS,"any"},
   {FALSE,FECutoffID,CODEC_ID_NONE,"any"},

   {TRUE,FEFrameSizeID,CODEC_ID_FLAC,"any"},
   {FALSE,FEFrameSizeID,CODEC_ID_NONE,"any"},

   {TRUE,FEProfileID,CODEC_ID_AAC,"any"},
   {FALSE,FEProfileID,CODEC_ID_NONE,"any"},

   {TRUE,FECompLevelID,CODEC_ID_FLAC,"any"},
   {FALSE,FECompLevelID,CODEC_ID_NONE,"any"},

   {TRUE,FEUseLPCID,CODEC_ID_FLAC,"any"},
   {FALSE,FEUseLPCID,CODEC_ID_NONE,"any"},

   {TRUE,FELPCCoeffsID,CODEC_ID_FLAC,"any"},
   {FALSE,FELPCCoeffsID,CODEC_ID_NONE,"any"},

   {TRUE,FEMinPredID,CODEC_ID_FLAC,"any"},
   {FALSE,FEMinPredID,CODEC_ID_NONE,"any"},

   {TRUE,FEMaxPredID,CODEC_ID_FLAC,"any"},
   {FALSE,FEMaxPredID,CODEC_ID_NONE,"any"},

   {TRUE,FEPredOrderID,CODEC_ID_FLAC,"any"},
   {FALSE,FEPredOrderID,CODEC_ID_NONE,"any"},

   {TRUE,FEMinPartOrderID,CODEC_ID_FLAC,"any"},
   {FALSE,FEMinPartOrderID,CODEC_ID_NONE,"any"},

   {TRUE,FEMaxPartOrderID,CODEC_ID_FLAC,"any"},
   {FALSE,FEMaxPartOrderID,CODEC_ID_NONE,"any"},

   {TRUE,FEMuxRateID,CODEC_ID_NONE,"mpeg"},
   {TRUE,FEMuxRateID,CODEC_ID_NONE,"vcd"},
   {TRUE,FEMuxRateID,CODEC_ID_NONE,"vob"},
   {TRUE,FEMuxRateID,CODEC_ID_NONE,"svcd"},
   {TRUE,FEMuxRateID,CODEC_ID_NONE,"dvd"},
   {FALSE,FEMuxRateID,CODEC_ID_NONE,"any"},

   {TRUE,FEPacketSizeID,CODEC_ID_NONE,"mpeg"},
   {TRUE,FEPacketSizeID,CODEC_ID_NONE,"vcd"},
   {TRUE,FEPacketSizeID,CODEC_ID_NONE,"vob"},
   {TRUE,FEPacketSizeID,CODEC_ID_NONE,"svcd"},
   {TRUE,FEPacketSizeID,CODEC_ID_NONE,"dvd"},
   {FALSE,FEPacketSizeID,CODEC_ID_NONE,"any"},

   {TRUE,FELanguageID,CODEC_ID_NONE,"matroska"},
   {TRUE,FELanguageID,CODEC_ID_NONE,"mov"},
   {TRUE,FELanguageID,CODEC_ID_NONE,"3gp"},
   {TRUE,FELanguageID,CODEC_ID_NONE,"mp4"},
   {TRUE,FELanguageID,CODEC_ID_NONE,"psp"},
   {TRUE,FELanguageID,CODEC_ID_NONE,"3g2"},
   {TRUE,FELanguageID,CODEC_ID_NONE,"ipod"},
   {TRUE,FELanguageID,CODEC_ID_NONE,"mpegts"},
   {FALSE,FELanguageID,CODEC_ID_NONE,"any"},

   {TRUE,FEBitReservoirID,CODEC_ID_MP3,"any"},
   {TRUE,FEBitReservoirID,CODEC_ID_WMAV1,"any"},
   {TRUE,FEBitReservoirID,CODEC_ID_WMAV2,"any"},
   {FALSE,FEBitReservoirID,CODEC_ID_NONE,"any"},

   {TRUE,FEVariableBlockLenID,CODEC_ID_WMAV1,"any"},
   {TRUE,FEVariableBlockLenID,CODEC_ID_WMAV2,"any"},
   {FALSE,FEVariableBlockLenID,CODEC_ID_NONE,"any"},

   {FALSE,FFmpegExportCtrlID(0),CODEC_ID_NONE,NULL}
};

/// Prediction order method - names. Labels are indices of this array.
static const wxChar *PredictionOrderMethodNames[] = { _("Estimate"), _("2-level"), _("4-level"), _("8-level"), _("Full search"), _("Log search")};

class FFmpegPresets;

/// Custom FFmpeg export dialog
class ExportFFmpegOptions : public wxDialog
{
public:

   ExportFFmpegOptions(wxWindow *parent);
   ~ExportFFmpegOptions();
   void PopulateOrExchange(ShuttleGui & S);
   void OnOK(wxCommandEvent& event);
   void OnFormatList(wxCommandEvent& event);
   void DoOnFormatList();
   void OnCodecList(wxCommandEvent& event);
   void DoOnCodecList();
   void OnAllFormats(wxCommandEvent& event);
   void OnAllCodecs(wxCommandEvent& event);
   void OnSavePreset(wxCommandEvent& event);
   void OnLoadPreset(wxCommandEvent& event);
   void OnDeletePreset(wxCommandEvent& event);
   void OnImportPresets(wxCommandEvent& event);
   void OnExportPresets(wxCommandEvent& event);

private:

   wxArrayString mShownFormatNames;
   wxArrayString mShownFormatLongNames;
   wxArrayString mShownCodecNames;
   wxArrayString mShownCodecLongNames;
   wxArrayString mFormatNames;
   wxArrayString mFormatLongNames;
   wxArrayString mCodecNames;
   wxArrayString mCodecLongNames;
   wxArrayString mProfileNames;
   wxArrayInt    mProfileLabels;
   wxArrayString mPredictionOrderMethodNames;;
   wxArrayInt    mPredictionOrderMethodLabels;

   wxChoice *mFormatChoice;
   wxChoice *mCodecChoice;

   wxListBox *mFormatList;
   wxListBox *mCodecList;

   wxStaticText *mFormatName;
   wxStaticText *mCodecName;

   wxChoice *mPresetChoice;
   wxComboBox *mPresetCombo;
   wxSpinCtrl *mBitrateSpin;
   wxSpinCtrl *mQualitySpin;
   wxSpinCtrl *mSampleRateSpin;
   wxTextCtrl *mLanguageText;
   wxTextCtrl *mTag;
   wxSpinCtrl *mCutoffSpin;
   wxCheckBox *mBitReservoirCheck;
   wxChoice *mProfileChoice;
   //wxSpinCtrl *mTrellisSpin; //trellis is only applicable for ADPCM...scrap it.
   wxSpinCtrl *mCompressionLevelSpin;
   wxSpinCtrl *mFrameSizeSpin;
   wxCheckBox *mUseLPCCheck;
   wxSpinCtrl *mLPCCoeffsPrecisionSpin;
   wxSpinCtrl *mMinPredictionOrderSpin;
   wxSpinCtrl *mMaxPredictionOrderSpin;
   wxChoice *mPredictionOrderMethodChoice;
   wxSpinCtrl *mMinPartitionOrderSpin;
   wxSpinCtrl *mMaxPartitionOrderSpin;
   wxSpinCtrl *mMuxRate;
   wxSpinCtrl *mPacketSize;

   wxButton *mOk;
   wxButton *mSavePreset;
   wxButton *mLoadPreset;
   wxButton *mDeletePreset;
   wxButton *mImportPresets;
   wxButton *mExportPresets;
   
   int mBitRateFromChoice;
   int mSampleRateFromChoice;

   FFmpegPresets *mPresets;

   wxArrayString *mPresetNames;

   /// Finds the format currently selected and returns it's name and description
   void FindSelectedFormat(wxString **name, wxString **longname);
   
   /// Finds the codec currently selected and returns it's name and description
   void FindSelectedCodec(wxString **name, wxString **longname);

   /// Retreives format list from libavformat
   void FetchFormatList();

   /// Retreives a list of formats compatible to codec
   ///\param id Codec ID
   ///\param selfmt format selected at the moment
   ///\return index of the selfmt in new format list or -1 if it is not in the list
   int FetchCompatibleFormatList(CodecID id, wxString *selfmt);

   /// Retreives codec list from libavcodec
   void FetchCodecList();

   /// Retreives a list of codecs compatible to format
   ///\param fmt Format short name
   ///\param id id of the codec selected at the moment
   ///\return index of the id in new codec list or -1 if it is not in the list
   int FetchCompatibleCodecList(const wxChar *fmt, CodecID id);

   /// Retreives list of presets from configuration file
   void FetchPresetList();

   // Enables/disables controls based on format/codec combination,
   // leaving only relevant controls enabled.
   // Hiding the controls may have been a better idea,
   // but it's hard to hide their text labels too  
   void EnableDisableControls(AVCodec *cdc, wxString *selfmt);
   DECLARE_EVENT_TABLE()
};

//----------------------------------------------------------------------------
// FFmpegPresets
//----------------------------------------------------------------------------

class FFmpegPreset
{
public:
   FFmpegPreset(wxString &name);
   ~FFmpegPreset();

   wxString *mPresetName;
   wxArrayString *mControlState;

};

WX_DECLARE_LIST(FFmpegPreset,FFmpegPresetList);

class FFmpegPresets : XMLTagHandler
{
public:
   FFmpegPresets();
   ~FFmpegPresets();

   wxArrayString *GetPresetList();
   void LoadPreset(ExportFFmpegOptions *parent, wxString &name);
   void SavePreset(ExportFFmpegOptions *parent, wxString &name);
   void DeletePreset(wxString &name);
   FFmpegPreset *FindPreset(wxString &name);

   void ImportPresets(wxString &filename);
   void ExportPresets(wxString &filename);

   bool HandleXMLTag(const wxChar *tag, const wxChar **attrs);
   XMLTagHandler *HandleXMLChild(const wxChar *tag);
   void WriteXMLHeader(XMLWriter &xmlFile);
   void WriteXML(XMLWriter &xmlFile);

private:

   FFmpegPresetList *mPresets;
};

#endif

#endif //__EXPORT_FFMPEG_DIALOGS_H__
