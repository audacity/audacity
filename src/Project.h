/**********************************************************************

  Audacity: A Digital Audio Editor

  Project.h

  Dominic Mazzoni

  In Audacity, the main window you work in is called a project.
  Projects can contain an arbitrary number of tracks of many
  different types, but if a project contains just one or two
  tracks then it can be saved in standard formats like WAV or AIFF.
  This window is the one that contains the menu bar (except on
  the Mac).

**********************************************************************/

#ifndef __AUDACITY_PROJECT__
#define __AUDACITY_PROJECT__

#include "Audacity.h"

#include "ClientData.h"
#include "Prefs.h"

#include "TrackPanelListener.h"
#include "AudioIOListener.h"
#include "toolbars/SelectionBarListener.h"
#include "toolbars/SpectralSelectionBarListener.h"

#include <memory>
#include <wx/frame.h> // to inherit

#include "import/ImportRaw.h" // defines TrackHolders

#include "xml/XMLTagHandler.h" // to inherit

const int AudacityProjectTimerID = 5200;

wxDECLARE_EXPORTED_EVENT(AUDACITY_DLL_API,
                         EVT_PROJECT_STATUS_UPDATE, wxCommandEvent);

class wxArrayString;
class wxWindow;
class wxScrollEvent;
class wxScrollBar;
class wxPanel;
class wxTimer;
class wxTimerEvent;

class AudacityProject;
class AutoSaveFile;
class ODLock;
class RecordingRecoveryHandler;
namespace ProjectFileIORegistry{ struct Entry; }
class TrackList;

class MeterPanel;

struct AudioIOStartStreamOptions;
struct UndoState;

enum class UndoPush : unsigned char;


AudacityProject *CreateNewAudacityProject();
AUDACITY_DLL_API AudacityProject *GetActiveProject();

void GetDefaultWindowRect(wxRect *defRect);
void GetNextWindowPlacement(wxRect *nextRect, bool *pMaximized, bool *pIconized);
bool IsWindowAccessible(wxRect *requestedRect);

class WaveTrack;
using WaveTrackArray = std::vector < std::shared_ptr < WaveTrack > >;


enum StatusBarField {
   stateStatusBarField = 1,
   mainStatusBarField = 2,
   rateStatusBarField = 3
};

// XML handler for <import> tag
class ImportXMLTagHandler final : public XMLTagHandler
{
 public:
   ImportXMLTagHandler(AudacityProject* pProject) { mProject = pProject; }

   bool HandleXMLTag(const wxChar *tag, const wxChar **attrs) override;
   XMLTagHandler *HandleXMLChild(const wxChar * WXUNUSED(tag))  override
      { return NULL; }

   // Don't want a WriteXML method because ImportXMLTagHandler is not a WaveTrack.
   // <import> tags are instead written by AudacityProject::WriteXML.
   //    void WriteXML(XMLWriter &xmlFile) /* not override */ { wxASSERT(false); }

 private:
   AudacityProject* mProject;
};

/// \brief an object of class AllProjects acts like a standard library
/// container, but refers to a global array of open projects.  So you can
/// iterate easily over shared pointers to them with range-for :
/// for (auto pProject : AllProjects{}) { ... }
/// The pointers are never null.
class AllProjects
{
   // Use shared_ptr to projects, because elsewhere we need weak_ptr
   using AProjectHolder = std::shared_ptr< AudacityProject >;
   using Container = std::vector< AProjectHolder >;
   static Container gAudacityProjects;

public:
   AllProjects() = default;

   size_t size() const;
   bool empty() const { return size() == 0; }

   using const_iterator = Container::const_iterator;
   const_iterator begin() const;
   const_iterator end() const;

   using const_reverse_iterator = Container::const_reverse_iterator;
   const_reverse_iterator rbegin() const;
   const_reverse_iterator rend() const;

   using value_type = Container::value_type;

   // If the project is present, remove it from the global array and return
   // a shared pointer, else return null.  This invalidates any iterators.
   value_type Remove( AudacityProject &project );

   // This invalidates iterators
   void Add( const value_type &pProject );

   /// In case you must iterate in a non-main thread, use this to prevent
   /// changes in the set of open projects
   static ODLock &Mutex();

   // Return true if all projects do close (always so if force == true)
   // But if return is false, that means the user cancelled close of at least
   // one un-saved project.
   static bool Close( bool force = false );
   static void SaveWindowSize();

   static bool Closing() { return sbClosing; }
   static void Reset() { sbWindowRectAlreadySaved = false; }

private:
   static bool sbClosing;
   static bool sbWindowRectAlreadySaved;
};

class Track;

// Container of various objects associated with the project, which is
// responsible for destroying them
using AttachedObjects = ClientData::Site<
   AudacityProject, ClientData::Base, ClientData::SkipCopying, std::shared_ptr
>;
// Container of pointers to various windows associated with the project, which
// is not responsible for destroying them -- wxWidgets handles that instead
using AttachedWindows = ClientData::Site<
   AudacityProject, wxWindow, ClientData::SkipCopying, wxWeakRef
>;

using ProjectWindow = AudacityProject;
class AUDACITY_DLL_API AudacityProject final : public wxFrame,
                                     public TrackPanelListener,
                                     public SelectionBarListener,
                                     public SpectralSelectionBarListener,
                                     public XMLTagHandler,
                                     public AudioIOListener,
                                     private PrefsListener
   , public AttachedObjects
   , public AttachedWindows
{
 public:
   static ProjectWindow &Get( AudacityProject &project ) { return project; }
   static const ProjectWindow &Get( const AudacityProject &project ) { return project; }
   static ProjectWindow *Find( AudacityProject *pProject ) { return pProject; }
   static const ProjectWindow *Find( const AudacityProject *pProject ) { return pProject; }
   AudacityProject &GetProject() { return *this; }
 
   using AttachedObjects = ::AttachedObjects;
   using AttachedWindows = ::AttachedWindows;

   AudacityProject(wxWindow * parent, wxWindowID id,
                   const wxPoint & pos, const wxSize & size);
   virtual ~AudacityProject();

   // Next available ID for sub-windows
   int NextWindowID();

   virtual void ApplyUpdatedTheme();

   void GetPlayRegion(double* playRegionStart, double *playRegionEnd);
   bool IsPlayRegionLocked() { return mLockPlayRegion; }
   void SetPlayRegionLocked(bool value) { mLockPlayRegion = value; }

   wxString GetProjectName() const;
   int GetAudioIOToken() const;
   bool IsAudioActive() const;
   void SetAudioIOToken(int token);

   bool IsActive() override;

   // File I/O

   /** @brief Show an open dialogue for opening audio files, and possibly other
    * sorts of files.
    *
    * The file type filter will automatically contain:
    * - "All files" with any extension or none,
    * - "All supported files" based on the file formats supported in this
    *   build of Audacity,
    * - All of the individual formats specified by the importer plug-ins which
    *   are built into this build of Audacity, each with the relevant file
    *   extensions for that format.
    * The dialogue will start in the DefaultOpenPath directory read from the
    * preferences, failing that the working directory. The file format filter
    * will be set to the DefaultOpenType from the preferences, failing that
    * the first format specified in the dialogue. These two parameters will
    * be saved to the preferences once the user has chosen a file to open.
    * @param extraformat Specify the name of an additional format to allow
    * opening in this dialogue. This string is free-form, but should be short
    * enough to fit in the file dialogue filter drop-down. It should be
    * translated.
    * @param extrafilter Specify the file extension(s) for the additional format
    * specified by extraformat. The patterns must include the wildcard (e.g.
    * "*.aup" not "aup" or ".aup"), separate multiple patters with a semicolon,
    * e.g. "*.aup;*.AUP" because patterns are case-sensitive. Do not add a
    * trailing semicolon to the string. This string should not be translated
    * @return Array of file paths which the user selected to open (multiple
    * selections allowed).
    */
   static wxArrayString ShowOpenDialog(const wxString &extraformat = {},
         const wxString &extrafilter = {});
   static bool IsAlreadyOpen(const FilePath &projPathName);
   static void OpenFiles(AudacityProject *proj);

   // Return the given project if that is not NULL, else create a project.
   // Then open the given project path.
   // But if an exception escapes this function, create no NEW project.
   static AudacityProject *OpenProject(
      AudacityProject *pProject,
      const FilePath &fileNameArg, bool addtohistory = true);

   struct ReadProjectResults
   {
      bool decodeError;
      bool parseSuccess;
      bool trackError;
      wxString errorString;
   };
   ReadProjectResults ReadProjectFile( const FilePath &fileName );

   void OpenFile(const FilePath &fileName, bool addtohistory = true);

private:
   void EnqueueODTasks();

public:
   using wxFrame::DetachMenuBar;

   bool WarnOfLegacyFile( );

   // If pNewTrackList is passed in non-NULL, it gets filled with the pointers to NEW tracks.
   bool Import(const FilePath &fileName, WaveTrackArray *pTrackArray = NULL);

   void ZoomAfterImport(Track *pTrack);

   // Takes array of unique pointers; returns array of shared
   std::vector< std::shared_ptr<Track> >
   AddImportedTracks(const FilePath &fileName,
                     TrackHolders &&newTracks);

   void CloseLock();

   bool Save();
   bool SaveAs(bool bWantSaveCopy = false, bool bLossless = false);
   bool SaveAs(const wxString & newFileName, bool bWantSaveCopy = false, bool addToHistory = true);
   // strProjectPathName is full path for aup except extension
   bool SaveCopyWaveTracks(const FilePath & strProjectPathName, bool bLossless = false);

private:
   bool DoSave(bool fromSaveAs, bool bWantSaveCopy, bool bLossless = false);
public:

   const FilePath &GetFileName() { return mFileName; }
   bool GetDirty() { return mDirty; }
   void SetProjectTitle( int number =-1);

   wxWindow *GetMainPage() { return mMainPage; }
   wxPanel *GetTopPanel() { return mTopPanel; }

   // Timer Record Auto Save/Export Routines
   bool SaveFromTimerRecording(wxFileName fnFile);
   bool IsProjectSaved();
   void ResetProjectToEmpty();
   void ResetProjectFileIO();

   // Routine to estimate how many minutes of recording time are left on disk
   int GetEstimatedRecordingMinsLeftOnDisk(long lCaptureChannels = 0);
   // Converts number of minutes to human readable format
   wxString GetHoursMinsString(int iMinutes);


   // Message Handlers

   void OnMenu(wxCommandEvent & event);
   void OnUpdateUI(wxUpdateUIEvent & event);

   void MacShowUndockedToolbars(bool show);
   void OnActivate(wxActivateEvent & event);

   void OnMouseEvent(wxMouseEvent & event);
   void OnIconize(wxIconizeEvent &event);
   void OnSize(wxSizeEvent & event);
   void OnShow(wxShowEvent & event);
   void OnMove(wxMoveEvent & event);
   void DoScroll();
   void OnScroll(wxScrollEvent & event);
   void OnCloseWindow(wxCloseEvent & event);
   void OnTimer(wxTimerEvent & event);
   void OnToolBarUpdate(wxCommandEvent & event);
   void OnOpenAudioFile(wxCommandEvent & event);

   void HandleResize();
   void UpdateLayout();
   void ZoomInByFactor( double ZoomFactor );
   void ZoomOutByFactor( double ZoomFactor );

   // Other commands
   
   int GetProjectNumber(){ return mProjectNo;};
   void UpdatePrefs() override;
   void RedrawProject(const bool bForceWaveTracks = false);
   void RefreshCursor();
   void Zoom(double level);
   void ZoomBy(double multiplier);
   void Rewind(bool shift);
   void SkipEnd(bool shift);


   // Scrollbars

   void OnScrollLeft();
   void OnScrollRight();

   void OnScrollLeftButton(wxScrollEvent & event);
   void OnScrollRightButton(wxScrollEvent & event);

   void FinishAutoScroll();
   void FixScrollbars();

   bool MayScrollBeyondZero() const;
   double ScrollingLowerBoundTime() const;
   // How many pixels are covered by the period from lowermost scrollable time, to the given time:
   // PRL: Bug1197: we seem to need to compute all in double, to avoid differing results on Mac
   double PixelWidthBeforeTime(double scrollto) const;
   void SetHorizontalThumb(double scrollto);

   // TrackPanel callback methods, overrides of TrackPanelListener
   void TP_DisplaySelection() override;

   void TP_RedrawScrollbars() override;
   void TP_ScrollLeft() override;
   void TP_ScrollRight() override;
   void TP_ScrollWindow(double scrollto) override;
   bool TP_ScrollUpDown(int delta) override;
   void TP_HandleResize() override;

   MeterPanel *GetPlaybackMeter();
   void SetPlaybackMeter(MeterPanel *playback);
   MeterPanel *GetCaptureMeter();
   void SetCaptureMeter(MeterPanel *capture);

   const wxString &GetStatus() const { return mLastMainStatusMessage; }
   void SetStatus(const wxString &msg);

   void OnStatusChange( wxCommandEvent& );

private:
   bool SnapSelection();

public:
   // SelectionBarListener callback methods

   double AS_GetRate() override;
   void AS_SetRate(double rate) override;
   int AS_GetSnapTo() override;
   void AS_SetSnapTo(int snap) override;
   const NumericFormatSymbol & AS_GetSelectionFormat() override;
   void AS_SetSelectionFormat(const NumericFormatSymbol & format) override;
   void AS_ModifySelection(double &start, double &end, bool done) override;

   // SpectralSelectionBarListener callback methods

   double SSBL_GetRate() const override;

   const NumericFormatSymbol & SSBL_GetFrequencySelectionFormatName() override;
   void SSBL_SetFrequencySelectionFormatName(const NumericFormatSymbol & formatName) override;

   const NumericFormatSymbol & SSBL_GetBandwidthSelectionFormatName() override;
   void SSBL_SetBandwidthSelectionFormatName(const NumericFormatSymbol & formatName) override;

   void SSBL_ModifySpectralSelection(double &bottom, double &top, bool done) override;

   void SetStateTo(unsigned int n);

   // XMLTagHandler callback methods

   bool HandleXMLTag(const wxChar *tag, const wxChar **attrs) override;
   XMLTagHandler *HandleXMLChild(const wxChar *tag) override;
   void WriteXML(
      XMLWriter &xmlFile, bool bWantSaveCopy) /* not override */;

   void WriteXMLHeader(XMLWriter &xmlFile) const;

   // Audio IO callback methods
   void OnAudioIORate(int rate) override;
   void OnAudioIOStartRecording() override;
   void OnAudioIOStopRecording() override;
   void OnAudioIONewBlockFiles(const AutoSaveFile & blockFileLog) override;

   bool UndoAvailable();
   bool RedoAvailable();

   void PushState(const wxString &desc, const wxString &shortDesc); // use UndoPush::AUTOSAVE
   void PushState(const wxString &desc, const wxString &shortDesc, UndoPush flags);
   void RollbackState();


 private:

   void OnThemeChange(wxCommandEvent & evt);
   void InitialState();

 public:
   void ModifyState(bool bWantsAutoSave);    // if true, writes auto-save file. Should set only if you really want the state change restored after
                                             // a crash, as it can take many seconds for large (eg. 10 track-hours) projects

   void PopState(const UndoState &state);

   void AutoSave();
   void DeleteCurrentAutoSaveFile();

 private:

   // The project's name and file info
   FilePath mFileName; // Note: extension-less
   bool mbLoadedFromAup;

   static int mProjectCounter;// global counter.
   int mProjectNo; // count when this project was created.

   std::shared_ptr<TrackList> mLastSavedTracks;

private:
   bool mDirty{ false };

   // Window elements

   wxString mLastMainStatusMessage;
   std::unique_ptr<wxTimer> mTimer;
   void RestartTimer();

   wxPanel *mTopPanel{};
   wxWindow * mMainPage;
   wxPanel * mMainPanel;
   wxScrollBar *mHsbar;
   wxScrollBar *mVsbar;

public:
   wxScrollBar &GetVerticalScrollBar() { return *mVsbar; }

private:
   int mNextWindowID;

   bool mAutoScrolling{ false };
   bool mActive{ true };
   bool mIconized;

   bool mShownOnce{ false };

   // Project owned meters
   MeterPanel *mPlaybackMeter{};
   MeterPanel *mCaptureMeter{};

 public:
   bool mbBusyImporting{ false }; // used to fix bug 584
   int mBatchMode{ 0 };// 0 means not, >0 means in batch mode.

   void SetNormalizedWindowState(wxRect pSizeAndLocation) {  mNormalizedWindowState = pSizeAndLocation;   }
   wxRect GetNormalizedWindowState() const { return mNormalizedWindowState;   }

   bool IsTimerRecordCancelled(){return mTimerRecordCanceled;}
   void SetTimerRecordCancelled(){mTimerRecordCanceled=true;}
   void ResetTimerRecordCancelled(){mTimerRecordCanceled=false;}

 private:
   int  mAudioIOToken{ -1 };

   bool mIsDeleting{ false };

public:
   bool IsBeingDeleted() const { return mIsDeleting; }
   void SetIsBeingDeleted() { mIsDeleting = true; }
private:

   bool mLockPlayRegion;

   std::unique_ptr<ImportXMLTagHandler> mImportXMLTagHandler;

   // Last auto-save file name and path (empty if none)
   wxString mAutoSaveFileName;

   // Are we currently auto-saving or not?
   bool mAutoSaving{ false };

   // Has this project been recovered from an auto-saved version
   bool mIsRecovered{ false };

   // The auto-save data dir the project has been recovered from
   wxString mRecoveryAutoSaveDataDir;

   // The handler that handles recovery of <recordingrecovery> tags
   std::unique_ptr<RecordingRecoveryHandler> mRecordingRecoveryHandler;

   // Dependencies have been imported and a warning should be shown on save
   bool mImportedDependencies{ false };

   FilePaths mStrOtherNamesArray; // used to make sure compressed file names are unique

   wxRect mNormalizedWindowState;

   //flag for cancellation of timer record.
   bool mTimerRecordCanceled{ false  };

   // Are we currently closing as the result of a menu command?
   bool mMenuClose{ false };

public:
   void SetMenuClose(bool value) { mMenuClose = value; }

private:
   bool mbInitializingScrollbar{ false };

public:
   class PlaybackScroller final : public wxEvtHandler
   {
   public:
      explicit PlaybackScroller(AudacityProject *project);

      enum class Mode {
         Off,
         Refresh,
         Pinned,
         Right,
      };

      Mode GetMode() const { return mMode; }
      void Activate(Mode mode)
      {
         mMode = mode;
      }

   private:
      void OnTimer(wxCommandEvent &event);

      AudacityProject *mProject;
      Mode mMode { Mode::Off };
   };

private:
   std::unique_ptr<PlaybackScroller> mPlaybackScroller;

   // Declared in this class so that they can have access to private members
   static XMLTagHandler *RecordingRecoveryFactory( AudacityProject &project );
   static ProjectFileIORegistry::Entry sRecoveryFactory;
   static XMLTagHandler *ImportHandlerFactory( AudacityProject &project );
   static ProjectFileIORegistry::Entry sImportHandlerFactory;

public:
   PlaybackScroller &GetPlaybackScroller() { return *mPlaybackScroller; }

   DECLARE_EVENT_TABLE()
};

inline wxFrame &GetProjectFrame( AudacityProject &project ) { return project; }
inline const wxFrame &GetProjectFrame( const AudacityProject &project ) {
   return project;
}
inline wxFrame *FindProjectFrame( AudacityProject *project ) {
   return project ? &GetProjectFrame( *project ) : nullptr;
}
inline const wxFrame *FindProjectFrame( const AudacityProject *project ) {
   return project ? &GetProjectFrame( *project ) : nullptr;
}

class ProjectSettings final
   : public ClientData::Base
   , private PrefsListener
{
public:
   static ProjectSettings &Get( AudacityProject &project );
   static const ProjectSettings &Get( const AudacityProject &project );
   
   ProjectSettings( AudacityProject &project );

   sampleFormat GetDefaultFormat() const { return mDefaultFormat; }

   double GetRate() const { return mRate; }
   void SetRate( double value ) { mRate = value; }

   bool GetTracksFitVerticallyZoomed() const { return mTracksFitVerticallyZoomed; } //lda
   void SetTracksFitVerticallyZoomed(bool flag) { mTracksFitVerticallyZoomed = flag; } //lda

   bool GetShowId3Dialog() const { return mShowId3Dialog; } //lda
   void SetShowId3Dialog(bool flag) { mShowId3Dialog = flag; } //lda

   bool GetNormalizeOnLoad() const { return mNormalizeOnLoad; } //lda
   void SetNormalizeOnLoad(bool flag) { mNormalizeOnLoad = flag; } //lda

   bool IsSyncLocked() const;
   void SetSyncLock(bool flag);
   
   // Snap To

   void SetSnapTo(int snap);
   int GetSnapTo() const;

   // Selection Format

   void SetSelectionFormat(const NumericFormatSymbol & format);
   const NumericFormatSymbol & GetSelectionFormat() const;

   // Spectral Selection Formats

   void SetFrequencySelectionFormatName(const NumericFormatSymbol & format);
   const NumericFormatSymbol & GetFrequencySelectionFormatName() const;

   void SetBandwidthSelectionFormatName(const NumericFormatSymbol & format);
   const NumericFormatSymbol & GetBandwidthSelectionFormatName() const;

   bool IsSoloSimple() const { return mSoloPref == wxT("Simple"); }
   bool IsSoloNone() const { return mSoloPref == wxT("None"); }

   bool EmptyCanBeDirty() const { return mEmptyCanBeDirty; }

   bool GetShowSplashScreen() const { return mShowSplashScreen; }

private:
   void UpdatePrefs() override;

   AudacityProject &mProject;

   NumericFormatSymbol mSelectionFormat;
   NumericFormatSymbol mFrequencySelectionFormatName;
   NumericFormatSymbol mBandwidthSelectionFormatName;

   wxString mSoloPref;

   double mRate;

   sampleFormat mDefaultFormat;
   int mSnapTo;
   
   bool mTracksFitVerticallyZoomed{ false };  //lda
   bool mShowId3Dialog{ true }; //lda
   bool mNormalizeOnLoad;  //lda
   bool mIsSyncLocked{ false };
   bool mEmptyCanBeDirty;
   bool mShowSplashScreen;
};

AudioIOStartStreamOptions DefaultPlayOptions( AudacityProject &project );
AudioIOStartStreamOptions DefaultSpeedPlayOptions( AudacityProject &project );

#endif
