/**********************************************************************

  Audacity: A Digital Audio Editor

  Export.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EXPORT__
#define __AUDACITY_EXPORT__

#include <functional>
#include <vector>
#include <wx/filename.h> // member variable
#include "Identifier.h"
#include "SampleFormat.h"
#include "wxPanelWrapper.h" // to inherit
#include "FileNames.h" // for FileTypes

#include "Registry.h"

class wxArrayString;
class FileDialogWrapper;
class wxFileCtrlEvent;
class wxMemoryDC;
class wxSimplebook;
class wxStaticText;
class AudacityProject;
class WaveTrack;
class Tags;
class TrackList;
namespace MixerOptions{ class Downmix; }
using MixerSpec = MixerOptions::Downmix;
class ProgressDialog;
class ShuttleGui;
using WaveTrackConstArray = std::vector < std::shared_ptr < const WaveTrack > >;

namespace BasicUI
{
class ProgressDialog;
enum class ProgressResult : unsigned;
}

struct AUDACITY_DLL_API FormatInfo
{
   wxString mFormat;
   TranslatableString mDescription;
   FileExtensions mExtensions;
   unsigned mMaxChannels;
   bool mCanMetaData;
};

//----------------------------------------------------------------------------
// ExportPlugin
//----------------------------------------------------------------------------
class AUDACITY_DLL_API ExportPlugin /* not final */
{
public:
   using ProgressResult = BasicUI::ProgressResult;

   ExportPlugin();
   virtual ~ExportPlugin();

   virtual int GetFormatCount() const = 0;
   virtual FormatInfo GetFormatInfo(int index) const = 0;

   virtual void OptionsCreate(ShuttleGui &S, int format) = 0;

   virtual bool CheckFileName(wxFileName &filename, int format = 0);
   /** @brief Exporter plug-ins may override this to specify the number
    * of channels in exported file. -1 for unspecified */
   virtual int SetNumExportChannels() { return -1; }

   /** \brief called to export audio into a file.
    *
    * @param pDialog To be initialized with pointer to a NEW ProgressDialog if
    * it was null, otherwise gives an existing dialog to be reused
   *  (working around a problem in wxWidgets for Mac; see bug 1600)
    * @param selectedOnly Set to true if all tracks should be mixed, to false
    * if only the selected tracks should be mixed and exported.
    * @param metadata A Tags object that will over-ride the one in *project and
    * be used to tag the file that is exported.
    * @param subformat Control which of the multiple formats this exporter is
    * capable of exporting should be used. Used where a single export plug-in
    * handles a number of related formats, but they have separate
    * entries in the Format drop-down list box. For example, the options to
    * export to "Other PCM", "AIFF 16 Bit" and "WAV 16 Bit" are all the same
    * libsndfile export plug-in, but with subformat set to 0, 1, and 2
    * respectively.
    * @return ProgressResult::Failed or ProgressResult::Cancelled if export
    * fails to complete for any reason, in which case this function is
    * responsible for alerting the user.  Otherwise ProgressResult::Success or
    * ProgressResult::Stopped
    */
   virtual ProgressResult Export(AudacityProject *project,
                       std::unique_ptr<BasicUI::ProgressDialog> &pDialog,
                       unsigned channels,
                       const wxFileNameWrapper &fName,
                       bool selectedOnly,
                       double t0,
                       double t1,
                       MixerSpec *mixerSpec = NULL,
                       const Tags *metadata = NULL,
                       int subformat = 0) = 0;

protected:
   
};

using ExportPluginArray = std::vector < std::unique_ptr< ExportPlugin > > ;

//----------------------------------------------------------------------------
// Exporter
//----------------------------------------------------------------------------

// For a file suffix change from the options.
wxDECLARE_EXPORTED_EVENT(AUDACITY_DLL_API,
   AUDACITY_FILE_SUFFIX_EVENT, wxCommandEvent);

class  AUDACITY_DLL_API Exporter final : public wxEvtHandler
{
   struct ExporterItem;
public:

   using ExportPluginFactory =
      std::function< std::unique_ptr< ExportPlugin >() >;

   // Objects of this type are statically constructed in files implementing
   // subclasses of ExportPlugin
   // Register factories, not plugin objects themselves, which allows them
   // to have some fresh state variables each time export begins again
   // and to compute translated strings for the current locale
   struct AUDACITY_DLL_API RegisteredExportPlugin
      : public Registry::RegisteredItem<ExporterItem>
   {
      RegisteredExportPlugin(
         const Identifier &id, // an internal string naming the plug-in
         const ExportPluginFactory&,
         const Registry::Placement &placement = { wxEmptyString, {} } );
   };

   Exporter( AudacityProject &project );
   virtual ~Exporter();

   void SetFileDialogTitle( const TranslatableString & DialogTitle );
   void SetDefaultFormat( const FileExtension & Format ){ mFormatName = Format;};

   bool Process(bool selectedOnly,
                double t0, double t1);
   bool Process(unsigned numChannels,
                const FileExtension &type, const wxString & filename,
                bool selectedOnly, double t0, double t1);

   bool Process(
      unsigned numChannels, const FileExtension& type, const wxString& filename,
      bool selectedOnly, double t0, double t1,
      std::unique_ptr<BasicUI::ProgressDialog>& progressDialog);

   const ExportPluginArray &GetPlugins();

   // Auto Export from Timer Recording
   bool ProcessFromTimerRecording(bool selectedOnly,
                                  double t0,
                                  double t1,
                                  wxFileName fnFile,
                                  int iFormat,
                                  int iSubFormat,
                                  int iFilterIndex);
   bool SetAutoExportOptions();
   int GetAutoExportFormat();
   int GetAutoExportSubFormat();
   int GetAutoExportFilterIndex();
   wxFileName GetAutoExportFileName();
   void OnExtensionChanged(wxCommandEvent &evt);
   void OnHelp(wxCommandEvent &evt);

private:
   struct AUDACITY_DLL_API ExporterItem final : Registry::SingleItem {
      static Registry::GroupItemBase &Registry();
      ExporterItem(
         const Identifier &id, const Exporter::ExportPluginFactory &factory );
      Exporter::ExportPluginFactory mFactory;
   };

   bool ExamineTracks();
   bool GetFilename();
   bool CheckFilename();
   bool CheckMix(bool prompt = true);
   bool ExportTracks(std::unique_ptr<BasicUI::ProgressDialog>& progressDialog);

   static void CreateUserPaneCallback(wxWindow *parent, wxUIntPtr userdata);
   void CreateUserPane(wxWindow *parent);
   void OnFilterChanged(wxFileCtrlEvent & evt);

private:
   FileExtension mFormatName;
   FileDialogWrapper *mDialog;
   TranslatableString mFileDialogTitle;
   AudacityProject *mProject;
   std::unique_ptr<MixerSpec> mMixerSpec;

   ExportPluginArray mPlugins;

   wxFileName mFilename;
   wxFileName mActualName;

   double mT0;
   double mT1;
   int mFilterIndex;
   int mFormat;
   int mSubFormat;
   int mNumSelected{};
   bool mMono{};
   unsigned mNumMono;
   unsigned mChannels;
   bool mSelectedOnly;

   wxSimplebook *mBook;

   DECLARE_EVENT_TABLE()
};

//----------------------------------------------------------------------------
// ExportMixerPanel
//----------------------------------------------------------------------------
class ExportMixerPanel final : public wxPanelWrapper
{
public:
   ExportMixerPanel( wxWindow *parent, wxWindowID id,
         MixerSpec *mixerSpec, wxArrayString trackNames,
         const wxPoint& pos = wxDefaultPosition,
         const wxSize& size = wxDefaultSize);
   virtual ~ExportMixerPanel();

   void OnMouseEvent(wxMouseEvent & event);
   void OnPaint(wxPaintEvent & event);

private:
   std::unique_ptr<wxBitmap> mBitmap;
   wxRect mEnvRect;
   int mWidth;
   int mHeight;
   MixerSpec *mMixerSpec;
   ArrayOf<wxRect> mChannelRects;
   ArrayOf<wxRect> mTrackRects;
   int mSelectedTrack, mSelectedChannel;
   wxArrayString mTrackNames;
   int mBoxWidth, mChannelHeight, mTrackHeight;

   void SetFont( wxMemoryDC &memDC, const wxString &text, int width, int height );
   double Distance( wxPoint &a, wxPoint &b );
   bool IsOnLine( wxPoint p, wxPoint la, wxPoint lb );

   DECLARE_EVENT_TABLE()
};

//----------------------------------------------------------------------------
// ExportMixerDialog
//----------------------------------------------------------------------------
class ExportMixerDialog final : public wxDialogWrapper
{
public:
   // constructors and destructors
   ExportMixerDialog( const TrackList * tracks, bool selectedOnly, unsigned maxNumChannels,
         wxWindow *parent, wxWindowID id, const TranslatableString &title,
         const wxPoint& pos = wxDefaultPosition,
         const wxSize& size = wxDefaultSize,
         long style = wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER );
   virtual ~ExportMixerDialog();

   MixerSpec* GetMixerSpec() { return mMixerSpec.get(); }

private:
   wxStaticText *mChannelsText;
   std::unique_ptr<MixerSpec> mMixerSpec;
   wxArrayString mTrackNames;

private:
   void OnOk( wxCommandEvent &event );
   void OnCancel( wxCommandEvent &event );
   void OnMixerPanelHelp( wxCommandEvent &event );
   void OnSlider( wxCommandEvent &event );
   void OnSize( wxSizeEvent &event );

private:
   DECLARE_EVENT_TABLE()
};

AUDACITY_DLL_API TranslatableString AudacityExportCaptionStr();
AUDACITY_DLL_API TranslatableString AudacityExportMessageStr();

/// We have many Export errors that are essentially anonymous
/// and are distinguished only by an error code number.
/// Rather than repeat the code, we have it just once.
AUDACITY_DLL_API void ShowExportErrorDialog(wxString ErrorCode,
   TranslatableString message = AudacityExportMessageStr(),
   const TranslatableString& caption = AudacityExportCaptionStr(),
   bool allowReporting = true);

AUDACITY_DLL_API
void ShowDiskFullExportErrorDialog(const wxFileNameWrapper &fileName);

#endif
