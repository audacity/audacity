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

#include <wx/hashmap.h>
#include <wx/listimpl.cpp>
#include "../xml/XMLFileReader.h"
#include "../FileNames.h"
#include "../widgets/wxPanelWrapper.h"


/// Identifiers for pre-set export types.
enum FFmpegExposedFormat
{
   FMT_M4A,
   FMT_AC3,
   FMT_AMRNB,
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
   unsigned maxchannels;      //!< how many channels this format could handle
   int canmetadata;           //!< !=0 if format supports metadata, -1 any avformat version, otherwise version support added
   bool canutf8;              //!< true if format supports metadata in UTF-8, false otherwise
   const wxChar *description; //!< format description (will be shown in export dialog)
   AVCodecID codecid;         //!< codec ID (see libavcodec/avcodec.h)
   bool compiledIn;           //!< support for this codec/format is compiled in (checked at runtime)
};


/// Describes format-codec compatibility
struct CompatibilityEntry
{
   const wxChar *fmt; //!< format, recognizeable by guess_format()
   AVCodecID codec;   //!< codec ID
};


/// AC3 export options dialog
class ExportFFmpegAC3Options final : public wxPanelWrapper
{
public:

   ExportFFmpegAC3Options(wxWindow *parent, int format);
   virtual ~ExportFFmpegAC3Options();

   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow();
   bool TransferDataFromWindow();

   /// Bit Rates supported by AC3 encoder
   static const int iAC3BitRates[];
   /// Sample Rates supported by AC3 encoder (must end with zero-element)
   /// It is not used in dialog anymore, but will be required later
   static const int iAC3SampleRates[];

private:

   wxArrayString mBitRateNames;
   wxArrayInt    mBitRateLabels;

   wxChoice *mBitRateChoice;
   int mBitRateFromChoice;
};

class ExportFFmpegAACOptions final : public wxPanelWrapper
{
public:

   ExportFFmpegAACOptions(wxWindow *parent, int format);
   virtual ~ExportFFmpegAACOptions();

   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow();
   bool TransferDataFromWindow();

private:

   wxSpinCtrl *mQualitySpin;
};

class ExportFFmpegAMRNBOptions final : public wxPanelWrapper
{
public:

   ExportFFmpegAMRNBOptions(wxWindow *parent, int format);
   virtual ~ExportFFmpegAMRNBOptions();

   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow();
   bool TransferDataFromWindow();

   static int iAMRNBBitRate[];

private:

   wxArrayString mBitRateNames;
   wxArrayInt    mBitRateLabels;

   wxChoice *mBitRateChoice;
   int mBitRateFromChoice;
};

class ExportFFmpegWMAOptions final : public wxPanelWrapper
{
public:

   ExportFFmpegWMAOptions(wxWindow *parent, int format);
   ~ExportFFmpegWMAOptions();

   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow();
   bool TransferDataFromWindow();

   static const int iWMASampleRates[];
   static const int iWMABitRate[];

private:

   wxArrayString mBitRateNames;
   wxArrayInt    mBitRateLabels;

   wxChoice *mBitRateChoice;
   int mBitRateFromChoice;
};

class ExportFFmpegCustomOptions final : public wxPanelWrapper
{
public:

   ExportFFmpegCustomOptions(wxWindow *parent, int format);
   ~ExportFFmpegCustomOptions();

   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow();
   bool TransferDataFromWindow();

   void OnOpen(wxCommandEvent & evt);

private:

   DECLARE_EVENT_TABLE()
};

/// Entry for the Applicability table
struct ApplicableFor
{
   bool                 enable;  //!< true if this control should be enabled, false otherwise
   int                  control; //!< control ID
   AVCodecID            codec;   //!< Codec ID
   const char          *format;  //!< Format short name
};

class FFmpegPresets;

/// Custom FFmpeg export dialog
class ExportFFmpegOptions final : public wxDialogWrapper
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

   // Static tables
   static CompatibilityEntry CompatibilityList[];
   static int iAACProfileValues[];
   static const wxChar *iAACProfileNames[];
   static ExposedFormat fmts[];
   static const int iAACSampleRates[];
   static ApplicableFor apptable[];
   static const wxChar *PredictionOrderMethodNames[];

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
   wxArrayString mPredictionOrderMethodNames;
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

   std::unique_ptr<FFmpegPresets> mPresets;

   wxArrayString mPresetNames;

   /// Finds the format currently selected and returns it's name and description
   void FindSelectedFormat(wxString **name, wxString **longname);

   /// Finds the codec currently selected and returns it's name and description
   void FindSelectedCodec(wxString **name, wxString **longname);

   /// Retreives format list from libavformat
   void FetchFormatList();

   /// Retreives a list of formats compatible to codec
   ///\param id Codec ID
   ///\param selfmt format selected at the moment
   ///\return index of the selfmt in NEW format list or -1 if it is not in the list
   int FetchCompatibleFormatList(AVCodecID id, wxString *selfmt);

   /// Retreives codec list from libavcodec
   void FetchCodecList();

   /// Retreives a list of codecs compatible to format
   ///\param fmt Format short name
   ///\param id id of the codec selected at the moment
   ///\return index of the id in NEW codec list or -1 if it is not in the list
   int FetchCompatibleCodecList(const wxChar *fmt, AVCodecID id);

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
   FFmpegPreset();
   ~FFmpegPreset();

   wxString mPresetName;
   wxArrayString mControlState;

};

WX_DECLARE_STRING_HASH_MAP(FFmpegPreset, FFmpegPresetMap);

class FFmpegPresets : XMLTagHandler
{
public:
   FFmpegPresets();
   ~FFmpegPresets();

   void GetPresetList(wxArrayString &list);
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

   FFmpegPresetMap mPresets;
   FFmpegPreset *mPreset; // valid during XML parsing only
   bool mAbortImport; // tells importer to ignore the rest of the import
};

#endif

#endif //__EXPORT_FFMPEG_DIALOGS_H__
