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

#include "DirManager.h"
#include "UndoManager.h"
#include "ViewInfo.h"
#include "TrackPanel.h"
#include "AudioIO.h"
#include "commands/CommandManager.h"
#include "effects/EffectManager.h"
#include "xml/XMLTagHandler.h"
#include "toolbars/SelectionBar.h"
#include "FreqWindow.h"

#include <wx/defs.h>
#include <wx/event.h>
#include <wx/log.h>
#include <wx/dragimag.h>
#include <wx/generic/dragimgg.h>
#include <wx/frame.h>
#include <wx/intl.h>
#include <wx/dcclient.h>

const int AudacityProjectTimerID = 5200;

class wxWindow;
class wxDialog;
class wxBoxSizer;
class wxScrollEvent;
class wxScrollBar;
class wxPanel;

class AudacityProject;
class Importer;
class ODLock;
class RecordingRecoveryHandler;
class TrackList;
class Tags;

// toolbar classes
class ControlToolBar;
class DeviceToolBar;
class EditToolBar;
class MeterToolBar;
class MixerToolBar;
class SelectionToolBar;
class Toolbar;
class ToolManager;
class ToolsToolBar;
class TranscriptionToolBar;

// windows and frames
class AdornedRulerPanel;
class HistoryWindow;
class LyricsWindow;
class MixerBoard;
class MixerBoardFrame;


AudacityProject *CreateNewAudacityProject();
AUDACITY_DLL_API AudacityProject *GetActiveProject();
void RedrawAllProjects();
void RefreshCursorForAllProjects();
AUDACITY_DLL_API void CloseAllProjects();

void GetDefaultWindowRect(wxRect *defRect);
void GetNextWindowPlacement(wxRect *nextRect, bool *pMaximized, bool *pIconized);
bool IsWindowAccessible(wxRect *requestedRect);

WX_DEFINE_ARRAY(AudacityProject *, AProjectArray);

extern AProjectArray gAudacityProjects;


WX_DEFINE_ARRAY(wxMenu *, MenuArray);

enum PlayMode {
   normalPlay,
   oneSecondPlay,
   loopedPlay
};

// XML handler for <import> tag
class ImportXMLTagHandler : public XMLTagHandler 
{
 public:
   ImportXMLTagHandler(AudacityProject* pProject) { mProject = pProject; };

   virtual bool HandleXMLTag(const wxChar *tag, const wxChar **attrs);
   virtual XMLTagHandler *HandleXMLChild(const wxChar * WXUNUSED(tag)) { return NULL; };
   
   // Don't want a WriteXML method because ImportXMLTagHandler is not a WaveTrack. 
   // <import> tags are instead written by AudacityProject::WriteXML.
   //    virtual void WriteXML(XMLWriter &xmlFile) { wxASSERT(false); } 

 private: 
   AudacityProject* mProject;
};

class AUDACITY_DLL_API AudacityProject:  public wxFrame,
                                     public TrackPanelListener,
                                     public SelectionBarListener,
                                     public XMLTagHandler,
                                     public AudioIOListener
{
 public:
   AudacityProject(wxWindow * parent, wxWindowID id,
                   const wxPoint & pos, const wxSize & size);
   virtual ~AudacityProject();

   TrackList *GetTracks() { return mTracks; };
   UndoManager *GetUndoManager() { return &mUndoManager; }

   sampleFormat GetDefaultFormat() { return mDefaultFormat; }

   double GetRate() { return mRate; }
   double GetZoom() { return mViewInfo.zoom; }
   double GetSel0() { return mViewInfo.sel0; }
   double GetSel1() { return mViewInfo.sel1; }

   Track *GetFirstVisible();
   void UpdateFirstVisible();

   void GetPlayRegion(double* playRegionStart, double *playRegionEnd);
   bool IsPlayRegionLocked() { return mLockPlayRegion; }
   
   void SetSel0(double);        //Added by STM 
   void SetSel1(double);        //Added by STM 

   bool Clipboard() { return (msClipT1 - msClipT0) > 0.0; }

   wxString GetName();
   DirManager *GetDirManager();
   TrackFactory *GetTrackFactory();
   AdornedRulerPanel *GetRulerPanel();
   Tags *GetTags();
   int GetAudioIOToken();
   void SetAudioIOToken(int token);

   bool IsActive();

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
   static wxArrayString ShowOpenDialog(wxString extraformat = wxEmptyString,
         wxString extrafilter = wxEmptyString);
   static bool IsAlreadyOpen(const wxString projPathName);
   static void OpenFiles(AudacityProject *proj);
   void OpenFile(wxString fileName, bool addtohistory = true);
   bool WarnOfLegacyFile( );

   // If pNewTrackList is passed in non-NULL, it gets filled with the pointers to new tracks.
   bool Import(wxString fileName, WaveTrackArray *pTrackArray = NULL); 

   void AddImportedTracks(wxString fileName,
                          Track **newTracks, int numTracks);
   void LockAllBlocks();
   void UnlockAllBlocks();
   bool Save(bool overwrite = true, bool fromSaveAs = false, bool bWantSaveCompressed = false);
   bool SaveAs(bool bWantSaveCompressed = false);
   bool SaveAs(const wxString newFileName, bool bWantSaveCompressed = false, bool addToHistory = true);
   #ifdef USE_LIBVORBIS
      bool SaveCompressedWaveTracks(const wxString strProjectPathName); // full path for aup except extension
   #endif
   void Clear();

   wxString GetFileName() { return mFileName; }
   bool GetDirty() { return mDirty; }
   void SetProjectTitle();

   TrackPanel * GetTrackPanel(){return mTrackPanel;};

   bool GetIsEmpty() { return mTracks->IsEmpty(); }

   bool GetTracksFitVerticallyZoomed() { return mTracksFitVerticallyZoomed; } //lda
   void SetTracksFitVerticallyZoomed(bool flag) { mTracksFitVerticallyZoomed = flag; } //lda

   bool GetShowId3Dialog() { return mShowId3Dialog; } //lda
   void SetShowId3Dialog(bool flag) { mShowId3Dialog = flag; } //lda

   bool GetNormalizeOnLoad() { return mNormalizeOnLoad; } //lda
   void SetNormalizeOnLoad(bool flag) { mNormalizeOnLoad = flag; } //lda

   /** \brief Sets the wxDialog that is being displayed 
     * Used by the custom dialog warning constructor and destructor
     */
   void SetMissingAliasFileDialog(wxDialog *dialog);
   
   /** \brief returns a pointer to the wxDialog if it is displayed, NULL otherwise.
     */
   wxDialog *GetMissingAliasFileDialog();

#include "Menus.h"

   CommandManager *GetCommandManager() { return &mCommandManager; }

   void RebuildMenuBar();
   void RebuildOtherMenus();
   void MayStartMonitoring();


   // Message Handlers

   void OnMenuEvent(wxMenuEvent & event);
   void OnMenu(wxCommandEvent & event);
   void OnUpdateUI(wxUpdateUIEvent & event);

   void OnActivate(wxActivateEvent & event);
   void OnMouseEvent(wxMouseEvent & event);
   void OnIconize(wxIconizeEvent &event);
   void OnSize(wxSizeEvent & event);
   void OnMove(wxMoveEvent & event);
   void OnScroll(wxScrollEvent & event);
   void OnCloseWindow(wxCloseEvent & event);
   void OnTimer(wxTimerEvent & event);
   void OnToolBarUpdate(wxCommandEvent & event);
   void OnOpenAudioFile(wxCommandEvent & event);
   void OnCaptureKeyboard(wxCommandEvent & event);
   void OnReleaseKeyboard(wxCommandEvent & event);
   void OnODTaskUpdate(wxCommandEvent & event);
   void OnODTaskComplete(wxCommandEvent & event);
   void OnTrackListUpdated(wxCommandEvent & event);
   bool HandleKeyDown(wxKeyEvent & event);
   bool HandleChar(wxKeyEvent & event);
   bool HandleKeyUp(wxKeyEvent & event);

   void HandleResize();
   void UpdateLayout();
   void ZoomInByFactor( double ZoomFactor );
   void ZoomOutByFactor( double ZoomFactor );

   // Other commands
   static TrackList *GetClipboardTracks();
   static void DeleteClipboard();
   static void DeleteAllProjectsDeleteLock();

   void UpdateMenus();
   void UpdatePrefs();
   void UpdatePrefsVariables();
   void RedrawProject(const bool bForceWaveTracks = false);
   void RefreshCursor();
   void SelectNone();
   void SelectAllIfNone();
   void Zoom(double level);
   void Rewind(bool shift);
   void SkipEnd(bool shift);
   void SetStop(bool bStopped);
   void EditByLabel( WaveTrack::EditFunction action, bool bSyncLockedTracks ); 
   void EditClipboardByLabel( WaveTrack::EditDestFunction action );
   bool IsSyncLocked();
   void SetSyncLock(bool flag);

   // "exclusive" mute means mute the chosen track and unmute all others.
   void HandleTrackMute(Track *t, const bool exclusive); 

   // Type of solo (standard or simple) follows the set preference, unless
   // alternate == true, which causes the opposite behavior.
   void HandleTrackSolo(Track *t, const bool alternate);

   // Snap To

   void SetSnapTo(int snap);
   int GetSnapTo();

   // Selection Format

   void SetSelectionFormat(const wxString & format);
   const wxString & GetSelectionFormat();

   // Scrollbars

   void OnScrollLeft();
   void OnScrollRight();

   void OnScrollLeftButton(wxScrollEvent & event);
   void OnScrollRightButton(wxScrollEvent & event);

   void FinishAutoScroll();
   void FixScrollbars();

   void SafeDisplayStatusMessage(const wxChar *msg);

   // TrackPanel access
   virtual wxSize GetTPTracksUsableArea();
   virtual void RefreshTPTrack(Track* pTrk, bool refreshbacking = true);
   
   // TrackPanel callback methods, overrides of TrackPanelListener
   virtual void TP_DisplaySelection();
   virtual void TP_DisplayStatusMessage(wxString msg);

   virtual int TP_GetCurrentTool();
   virtual ToolsToolBar * TP_GetToolsToolBar();
   virtual ControlToolBar * TP_GetControlToolBar();

   virtual void TP_OnPlayKey();
   virtual void TP_PushState(wxString longDesc, wxString shortDesc,
                             int flags);
   virtual void TP_ModifyState(bool bWantsAutoSave);    // if true, writes auto-save file. Should set only if you really want the state change restored after
                                                        // a crash, as it can take many seconds for large (eg. 10 track-hours) projects
   virtual void TP_RedrawScrollbars();
   virtual void TP_ScrollLeft();
   virtual void TP_ScrollRight();
   virtual void TP_ScrollWindow(double scrollto);
   virtual void TP_ScrollUpDown(int delta);
   virtual void TP_HandleResize();

   // ToolBar

   // In the GUI, ControlToolBar appears as the "Transport Toolbar". "Control Toolbar" is historic.
   ControlToolBar *GetControlToolBar(); 

   DeviceToolBar *GetDeviceToolBar();
   EditToolBar *GetEditToolBar();
   MeterToolBar *GetMeterToolBar();
   MixerToolBar *GetMixerToolBar();
   SelectionBar *GetSelectionBar();
   ToolsToolBar *GetToolsToolBar();
   TranscriptionToolBar *GetTranscriptionToolBar();

   LyricsWindow* GetLyricsWindow() { return mLyricsWindow; };
   MixerBoard* GetMixerBoard() { return mMixerBoard; };

   // SelectionBar callback methods

   virtual double AS_GetRate();
   virtual void AS_SetRate(double rate);
   virtual int AS_GetSnapTo();
   virtual void AS_SetSnapTo(int snap);
   virtual const wxString & AS_GetSelectionFormat();
   virtual void AS_SetSelectionFormat(const wxString & format);
   virtual void AS_ModifySelection(double &start, double &end, bool done);

   void SetStateTo(unsigned int n);

   // XMLTagHandler callback methods

   virtual bool HandleXMLTag(const wxChar *tag, const wxChar **attrs);
   virtual XMLTagHandler *HandleXMLChild(const wxChar *tag);
   virtual void WriteXML(XMLWriter &xmlFile);

   void WriteXMLHeader(XMLWriter &xmlFile);

   PlayMode mLastPlayMode;
   ViewInfo mViewInfo;

   wxWindow *HasKeyboardCapture();
   void CaptureKeyboard(wxWindow *h);
   void ReleaseKeyboard(wxWindow *h);
   
   // Audio IO callback methods
   virtual void OnAudioIORate(int rate);
   virtual void OnAudioIOStartRecording();
   virtual void OnAudioIOStopRecording();
   virtual void OnAudioIONewBlockFiles(const wxString& blockFileLog);

   // Command Handling
   bool TryToMakeActionAllowed( wxUint32 & flags, wxUint32 flagsRqd, wxUint32 mask );

   ///Prevents delete from external thread - for e.g. use of GetActiveProject 
   static void AllProjectsDeleteLock();
   static void AllProjectsDeleteUnlock();
   
   void PushState(wxString desc, wxString shortDesc,
                  int flags = PUSH_AUTOSAVE | PUSH_CALC_SPACE);

 private:

   void ClearClipboard();
   void InitialState();
   void ModifyState(bool bWantsAutoSave);    // if true, writes auto-save file. Should set only if you really want the state change restored after
                                             // a crash, as it can take many seconds for large (eg. 10 track-hours) projects
   void PopState(TrackList * l);
   
   void UpdateLyrics();
   void UpdateMixerBoard();
   
   void GetRegionsByLabel( Regions &regions );
   
   void AutoSave();
   void DeleteCurrentAutoSaveFile();
   
   static bool GetCacheBlockFiles();

 public:
   bool IsSoloSimple() { return mSoloPref == wxT("Simple"); };
   bool IsSoloNone() { return mSoloPref == wxT("None"); };

 private:

   // The project's name and file info
   wxString mFileName;
   DirManager *mDirManager; // MM: DirManager now created dynamically

   double mRate;
   sampleFormat mDefaultFormat;

   // Recent files
   wxMenu *mRecentFilesMenu;

   // Tags (artist name, song properties, MP3 ID3 info, etc.)
   Tags *mTags;

   // List of tracks and display info
   TrackList *mTracks;

   int mSnapTo;
   wxString mSelectionFormat;

   TrackList *mLastSavedTracks;

   // Clipboard (static because it is shared by all projects)
   static TrackList *msClipboard;
   static AudacityProject *msClipProject;
   static double msClipT0;
   static double msClipT1;

   //shared by all projects
   static ODLock *msAllProjectDeleteMutex;

   // History/Undo manager
   UndoManager mUndoManager;
   bool mDirty;

   // Commands

   CommandManager mCommandManager;

   wxUint32 mLastFlags;

   // Window elements

   wxTimer *mTimer;
   long mLastStatusUpdateTime;

   wxStatusBar *mStatusBar;

   AdornedRulerPanel *mRuler;
   TrackPanel *mTrackPanel;
   TrackFactory *mTrackFactory;
   wxPanel * mMainPanel;
   wxScrollBar *mHsbar;
   wxScrollBar *mVsbar;
   bool mAutoScrolling;
   bool mActive;
   bool mIconized;

   HistoryWindow *mHistoryWindow;
   LyricsWindow* mLyricsWindow;
   MixerBoard* mMixerBoard;
   MixerBoardFrame* mMixerBoardFrame;

   FreqWindow *mFreqWindow;

   // dialog for missing alias warnings
   wxDialog            *mAliasMissingWarningDialog;

 public:
   ToolManager *mToolManager;
   bool mShowSplashScreen;
   wxString mHelpPref;
   wxString mSoloPref;
   bool mbBusyImporting; // used to fix bug 584

   void SetNormalizedWindowState(wxRect pSizeAndLocation) {  mNormalizedWindowState = pSizeAndLocation;   }
   wxRect GetNormalizedWindowState() const { return mNormalizedWindowState;   }

   bool IsTimerRecordCancelled(){return mTimerRecordCanceled;}
   void ResetTimerRecordFlag(){mTimerRecordCanceled=false;}
 private:
   //sort method used by OnSortName and OnSortTime
   //currently only supported flags are kAudacitySortByName and kAudacitySortByName
   //in the future we might have 0x01 as sort ascending and we can bit or it
#define kAudacitySortByTime (1 << 1)
#define kAudacitySortByName (1 << 2)
   void SortTracks(int flags);

   int  mAudioIOToken;

   bool mIsDeleting;
   bool mTracksFitVerticallyZoomed;  //lda
   bool mNormalizeOnLoad;  //lda
   bool mShowId3Dialog; //lda
   bool mEmptyCanBeDirty;
   bool mSelectAllOnNone;
   
   bool mIsSyncLocked;

   bool mLockPlayRegion;

   // See AudacityProject::OnActivate() for an explanation of this.
   wxWindow *mLastFocusedWindow;

   wxWindow *mKeyboardCaptured;

   ImportXMLTagHandler* mImportXMLTagHandler;

   // Last auto-save file name and path (empty if none)
   wxString mAutoSaveFileName;
   
   // Are we currently auto-saving or not?
   bool mAutoSaving;

   // Has this project been recovered from an auto-saved version
   bool mIsRecovered;
   
   // The auto-save data dir the project has been recovered from
   wxString mRecoveryAutoSaveDataDir;
   
   // The handler that handles recovery of <recordingrecovery> tags
   RecordingRecoveryHandler* mRecordingRecoveryHandler;

   // Dependencies have been imported and a warning should be shown on save
   bool mImportedDependencies;

   bool mWantSaveCompressed;
   wxArrayString mStrOtherNamesArray; // used to make sure compressed file names are unique
   
   // Last effect applied to this project
   Effect *mLastEffect;
   int mLastEffectType;

   // The screenshot class needs to access internals
   friend class ScreenshotCommand;

   wxRect mNormalizedWindowState;
   
   //flag for cancellation of timer record.
   bool mTimerRecordCanceled;

   // Are we currently closing as the result of a menu command?
   bool mMenuClose;

 public:
    DECLARE_EVENT_TABLE()
};

typedef void (AudacityProject::*audCommandFunction)();
typedef void (AudacityProject::*audCommandKeyFunction)(const wxEvent *);
typedef void (AudacityProject::*audCommandListFunction)(int);

// Previously this was in menus.cpp, and the declaration of the
// command functor was not visible anywhere else.
class AUDACITY_DLL_API AudacityProjectCommandFunctor : public CommandFunctor
{
public:
   AudacityProjectCommandFunctor(AudacityProject *project,
      audCommandFunction commandFunction);
   AudacityProjectCommandFunctor(AudacityProject *project,
      audCommandKeyFunction commandFunction);
   AudacityProjectCommandFunctor(AudacityProject *project,
      audCommandListFunction commandFunction);
   AudacityProjectCommandFunctor(AudacityProject *project,
      audCommandListFunction commandFunction,
      wxArrayInt explicitIndices);
   virtual void operator()(int index = 0, const wxEvent *evt = NULL);
private:
   AudacityProject *mProject;
   audCommandFunction mCommandFunction;
   audCommandKeyFunction mCommandKeyFunction;
   audCommandListFunction mCommandListFunction;
   wxArrayInt mExplicitIndices;
};

#endif

