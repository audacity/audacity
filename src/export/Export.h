/**********************************************************************

  Audacity: A Digital Audio Editor

  Export.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EXPORT__
#define __AUDACITY_EXPORT__

#include "../MemoryX.h"
#include <vector>
#include <wx/dialog.h>
#include <wx/dynarray.h>
#include <wx/filename.h>
#include <wx/simplebook.h>
#include "../Tags.h"
#include "../SampleFormat.h"
#include "../widgets/wxPanelWrapper.h"

#include "FileDialog.h"

class wxMemoryDC;
class wxStaticText;
class AudacityProject;
class DirManager;
class WaveTrack;
class TrackList;
class MixerSpec;
class TimeTrack;
class Mixer;
class WaveTrackConstArray;

class AUDACITY_DLL_API FormatInfo
{
   public:
      FormatInfo(){};
      ~FormatInfo(){};
      wxString mFormat;
      wxString mDescription;
      // wxString mExtension;
      wxArrayString mExtensions;
      wxString mMask;
      unsigned mMaxChannels;
      bool mCanMetaData;
};

WX_DECLARE_USER_EXPORTED_OBJARRAY(FormatInfo, FormatInfoArray, AUDACITY_DLL_API);

//----------------------------------------------------------------------------
// ExportPlugin
//----------------------------------------------------------------------------
class AUDACITY_DLL_API ExportPlugin /* not final */
{
public:

   ExportPlugin();
   virtual ~ExportPlugin();

   int AddFormat();
   void SetFormat(const wxString & format, int index);
   void SetDescription(const wxString & description, int index);
   void AddExtension(const wxString &extension,int index);
   void SetExtensions(const wxArrayString & extensions, int index);
   void SetMask(const wxString & mask, int index);
   void SetMaxChannels(unsigned maxchannels, unsigned index);
   void SetCanMetaData(bool canmetadata, int index);

   virtual int GetFormatCount();
   virtual wxString GetFormat(int index);
   virtual wxString GetDescription(int index);
   /** @brief Return the (first) file name extension for the sub-format.
    * @param index The sub-format for which the extension is wanted */
   virtual wxString GetExtension(int index = 0);
   /** @brief Return all the file name extensions used for the sub-format.
    * @param index the sub-format for which the extension is required */
   virtual wxArrayString GetExtensions(int index = 0);
   virtual wxString GetMask(int index);
   virtual unsigned GetMaxChannels(int index);
   virtual bool GetCanMetaData(int index);

   virtual bool IsExtension(const wxString & ext, int index);

   virtual bool DisplayOptions(wxWindow *parent, int format = 0);
   
   // Precondition: parent != NULL
   virtual wxWindow *OptionsCreate(wxWindow *parent, int format) = 0;

   virtual bool CheckFileName(wxFileName &filename, int format = 0);
   /** @brief Exporter plug-ins may override this to specify the number
    * of channels in exported file. -1 for unspecified */
   virtual int SetNumExportChannels() { return -1; }

   /** \brief called to export audio into a file.
    *
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
    */
   virtual int Export(AudacityProject *project,
                       unsigned channels,
                       const wxString &fName,
                       bool selectedOnly,
                       double t0,
                       double t1,
                       MixerSpec *mixerSpec = NULL,
                       const Tags *metadata = NULL,
                       int subformat = 0) = 0;

protected:
   std::unique_ptr<Mixer> CreateMixer(const WaveTrackConstArray &inputTracks,
         const TimeTrack *timeTrack,
         double startTime, double stopTime,
         unsigned numOutChannels, int outBufferSize, bool outInterleaved,
         double outRate, sampleFormat outFormat,
         bool highQuality = true, MixerSpec *mixerSpec = NULL);

private:
   FormatInfoArray mFormatInfos;
};

using ExportPluginArray = std::vector < movable_ptr< ExportPlugin > > ;
WX_DEFINE_USER_EXPORTED_ARRAY_PTR(wxWindow *, WindowPtrArray, class AUDACITY_DLL_API);

//----------------------------------------------------------------------------
// Exporter
//----------------------------------------------------------------------------
class  AUDACITY_DLL_API Exporter final : public wxEvtHandler
{
public:

   Exporter();
   virtual ~Exporter();

   void SetFileDialogTitle( const wxString & DialogTitle );
   void SetDefaultFormat( const wxString & Format ){ mFormatName = Format;};
   void RegisterPlugin(movable_ptr<ExportPlugin> &&plugin);

   bool Process(AudacityProject *project, bool selectedOnly,
                double t0, double t1);
   bool Process(AudacityProject *project, unsigned numChannels,
                const wxChar *type, const wxString & filename,
                bool selectedOnly, double t0, double t1);

   void DisplayOptions(int index);
   int FindFormatIndex(int exportindex);

   const ExportPluginArray &GetPlugins();

   // Auto Export from Timer Recording
   bool ProcessFromTimerRecording(AudacityProject *project,
                                  bool selectedOnly,
                                  double t0,
                                  double t1,
                                  wxFileName fnFile,
                                  int iFormat,
                                  int iSubFormat,
                                  int iFilterIndex);
   bool SetAutoExportOptions(AudacityProject *project);
   int GetAutoExportFormat();
   int GetAutoExportSubFormat();
   int GetAutoExportFilterIndex();
   wxFileName GetAutoExportFileName();

private:
   bool ExamineTracks();
   bool GetFilename();
   bool CheckFilename();
   bool CheckMix();
   bool ExportTracks();

   static void CreateUserPaneCallback(wxWindow *parent, wxUIntPtr userdata);
   void CreateUserPane(wxWindow *parent);
   void OnFilterChanged(wxFileCtrlEvent & evt);

private:
   wxString mFormatName;
   FileDialog *mDialog;
   wxString mFileDialogTitle;
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
   int mNumSelected;
   unsigned mNumLeft;
   unsigned mNumRight;
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
   ExportMixerPanel( MixerSpec *mixerSpec, wxArrayString trackNames,
         wxWindow *parent, wxWindowID id, const wxPoint& pos = wxDefaultPosition,
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
   wxRect *mChannelRects;
   wxRect *mTrackRects;
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
         wxWindow *parent, wxWindowID id, const wxString &title,
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
   void OnSlider( wxCommandEvent &event );
   void OnSize( wxSizeEvent &event );

private:
   DECLARE_EVENT_TABLE()
};
#endif
