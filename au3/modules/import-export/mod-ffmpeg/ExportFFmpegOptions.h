/**********************************************************************

   Audacity: A Digital Audio Editor

   ExportFFmpegOptions.h

   Audacity(R) is copyright (c) 1999-2010 Audacity Team.
   License: GPL v2 or later.  See License.txt.

   LRN

   Vitaly Sverchinsky split from ExportFFmpegDialogs.h

**********************************************************************/

#pragma once

#include "FFmpegFunctions.h"
#include "wxPanelWrapper.h"

class FFmpegPresets;
class ShuttleGui;
class wxListBox;
class wxStaticText;
class wxComboBox;

/// Identifiers for pre-set export types.
enum FFmpegExposedFormat
{
    FMT_M4A,
    FMT_AC3,
    FMT_AMRNB,
#ifdef SHOW_FFMPEG_OPUS_EXPORT
    FMT_OPUS,
#endif
    FMT_WMA2,
    FMT_OTHER,
    FMT_LAST
};

/// Entry for the Applicability table
struct ApplicableFor
{
    bool enable;                 //!< true if this control should be enabled, false otherwise
    int control;                 //!< control ID
    AudacityAVCodecID codec;     //!< Codec ID
    const char* format;          //!< Format short name
};

/// Describes format-codec compatibility
struct CompatibilityEntry
{
    const wxChar* fmt; //!< format, recognizable by guess_format()
    AudacityAVCodecID codec; //!< codec ID
};

/// Describes export type
struct ExposedFormat
{
    FFmpegExposedFormat fmtid; //!< one of the FFmpegExposedFormat
    const wxChar* name;       //!< format name (internal, should be unique; if not - export dialog may show unusual behaviour)
    const FileExtension extension;  //!< default extension for this format. More extensions may be added later via AddExtension.
    const wxChar* shortname;  //!< used to guess the format
    unsigned maxchannels;     //!< how many channels this format could handle
    const int canmetadata;          //!< !=0 if format supports metadata, AV_CANMETA any avformat version, otherwise version support added
    bool canutf8;             //!< true if format supports metadata in UTF-8, false otherwise
    const TranslatableString description; //!< format description (will be shown in export dialog)
    AudacityAVCodecID codecid;        //!< codec ID (see libavcodec/avcodec.h)
    bool compiledIn;          //!< support for this codec/format is compiled in (checked at runtime)
};

/// Custom FFmpeg export dialog
class ExportFFmpegOptions final : public wxDialogWrapper
{
public:

    ExportFFmpegOptions(wxWindow* parent);
    ~ExportFFmpegOptions();
    void PopulateOrExchange(ShuttleGui& S);
    void OnOK(wxCommandEvent& event);
    void OnGetURL(wxCommandEvent& event);
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
    bool SavePreset(bool bCheckForOverwrite);

    // Static tables
    static CompatibilityEntry CompatibilityList[];
    static ExposedFormat fmts[];
    static const int iAACSampleRates[];
    static ApplicableFor apptable[];

private:

    wxArrayString mShownFormatNames;
    wxArrayString mShownFormatLongNames;
    wxArrayString mShownCodecNames;
    wxArrayString mShownCodecLongNames;
    wxArrayStringEx mFormatNames;
    wxArrayString mFormatLongNames;
    wxArrayStringEx mCodecNames;
    wxArrayString mCodecLongNames;

    wxListBox* mFormatList;
    wxListBox* mCodecList;

    wxStaticText* mFormatName;
    wxStaticText* mCodecName;

    wxComboBox* mPresetCombo;

    int mBitRateFromChoice;
    int mSampleRateFromChoice;

    std::unique_ptr<FFmpegPresets> mPresets;

    wxArrayStringEx mPresetNames;

    std::shared_ptr<FFmpegFunctions> mFFmpeg;

    /// Finds the format currently selected and returns its name and description
    void FindSelectedFormat(wxString** name, wxString** longname);

    /// Finds the codec currently selected and returns its name and description
    void FindSelectedCodec(wxString** name, wxString** longname);

    /// Retrieves format list from libavformat
    void FetchFormatList();

    /// Retrieves a list of formats compatible to codec
    ///\param id Codec ID
    ///\param selfmt format selected at the moment
    ///\return index of the selfmt in NEW format list or -1 if it is not in the list
    int FetchCompatibleFormatList(AudacityAVCodecID id, wxString* selfmt);

    /// Retrieves codec list from libavcodec
    void FetchCodecList();

    /// Retrieves a list of codecs compatible to format
    ///\param fmt Format short name
    ///\param id id of the codec selected at the moment
    ///\return index of the id in NEW codec list or -1 if it is not in the list
    int FetchCompatibleCodecList(const wxChar* fmt, AudacityAVCodecID id);

    /// Retrieves list of presets from configuration file
    void FetchPresetList();

    bool ReportIfBadCombination();

    // Enables/disables controls based on format/codec combination,
    // leaving only relevant controls enabled.
    // Hiding the controls may have been a better idea,
    // but it's hard to hide their text labels too
    void EnableDisableControls(AVCodecWrapper* cdc, wxString* selfmt);
    DECLARE_EVENT_TABLE()
};
