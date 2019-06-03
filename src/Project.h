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

#include <memory>
#include <wx/frame.h> // to inherit

#include "xml/XMLTagHandler.h" // to inherit

wxDECLARE_EXPORTED_EVENT(AUDACITY_DLL_API,
                         EVT_PROJECT_STATUS_UPDATE, wxCommandEvent);

class wxWindow;
class wxScrollEvent;
class wxScrollBar;
class wxPanel;

class AudacityProject;
class ODLock;
class TrackList;


AUDACITY_DLL_API AudacityProject *GetActiveProject();
// For use by ProjectManager only:
extern void SetActiveProject(AudacityProject * project);

void GetDefaultWindowRect(wxRect *defRect);
void GetNextWindowPlacement(wxRect *nextRect, bool *pMaximized, bool *pIconized);
bool IsWindowAccessible(wxRect *requestedRect);

enum StatusBarField {
   stateStatusBarField = 1,
   mainStatusBarField = 2,
   rateStatusBarField = 3
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

   static bool Closing() { return sbClosing; }

private:
   static bool sbClosing;
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
                                     public XMLTagHandler,
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

   bool IsActive() override;

   struct ReadProjectResults
   {
      bool decodeError;
      bool parseSuccess;
      bool trackError;
      wxString errorString;
   };
   ReadProjectResults ReadProjectFile( const FilePath &fileName );

   void EnqueueODTasks();

   using wxFrame::DetachMenuBar;

   bool WarnOfLegacyFile( );

   void ZoomAfterImport(Track *pTrack);

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
   void SetFileName( const FilePath &value ) { mFileName = value; }

   const FilePath &GetAutoSaveFileName() { return mAutoSaveFileName; }
   void SetProjectTitle( int number =-1);

   wxWindow *GetMainPage() { return mMainPage; }
   wxPanel *GetTopPanel() { return mTopPanel; }

   // Timer Record Auto Save/Export Routines
   bool SaveFromTimerRecording(wxFileName fnFile);
   bool IsProjectSaved();

   void ResetProjectFileIO();


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
   void OnToolBarUpdate(wxCommandEvent & event);

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

   const wxString &GetStatus() const { return mLastMainStatusMessage; }
   void SetStatus(const wxString &msg);

public:

   // XMLTagHandler callback methods

   bool HandleXMLTag(const wxChar *tag, const wxChar **attrs) override;
   XMLTagHandler *HandleXMLChild(const wxChar *tag) override;
   void WriteXML(
      XMLWriter &xmlFile, bool bWantSaveCopy) /* not override */;

   void WriteXMLHeader(XMLWriter &xmlFile) const;

 private:

   void OnThemeChange(wxCommandEvent & evt);

 public:
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

   // Window elements

   wxString mLastMainStatusMessage;

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

 public:
   bool mbBusyImporting{ false }; // used to fix bug 584
   int mBatchMode{ 0 };// 0 means not, >0 means in batch mode.

   void SetNormalizedWindowState(wxRect pSizeAndLocation) {  mNormalizedWindowState = pSizeAndLocation;   }
   wxRect GetNormalizedWindowState() const { return mNormalizedWindowState;   }

 private:
   bool mIsDeleting{ false };

public:
   bool IsBeingDeleted() const { return mIsDeleting; }
   void SetIsBeingDeleted() { mIsDeleting = true; }
   bool IsRecovered() const { return mIsRecovered; }
   void SetImportedDependencies( bool value ) { mImportedDependencies = value; }
   void SetLoadedFromAup( bool value ) { mbLoadedFromAup = value; }
private:

   bool mLockPlayRegion;

   // Last auto-save file name and path (empty if none)
   FilePath mAutoSaveFileName;

   // Are we currently auto-saving or not?
   bool mAutoSaving{ false };

   // Has this project been recovered from an auto-saved version
   bool mIsRecovered{ false };

   // The auto-save data dir the project has been recovered from
   wxString mRecoveryAutoSaveDataDir;

   // Dependencies have been imported and a warning should be shown on save
   bool mImportedDependencies{ false };

   FilePaths mStrOtherNamesArray; // used to make sure compressed file names are unique

   wxRect mNormalizedWindowState;

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

// TitleRestorer restores project window titles to what they were, in its destructor.
class TitleRestorer{
public:
   TitleRestorer(AudacityProject * p );
   ~TitleRestorer();
   wxString sProjNumber;
   wxString sProjName;
   size_t UnnamedCount;
};

#endif
