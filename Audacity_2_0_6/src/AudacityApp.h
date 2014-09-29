/**********************************************************************

  Audacity: A Digital Audio Editor

  AudacityApp.h

  Dominic Mazzoni

  This is the main source file for Audacity which handles
  initialization and termination by subclassing wxApp.

**********************************************************************/

#ifndef __AUDACITY_APP__
#define __AUDACITY_APP__

#include "Audacity.h"

#include <wx/app.h>
#include <wx/dir.h>
#include <wx/event.h>
#include <wx/docview.h>
#include <wx/intl.h>
#include <wx/snglinst.h>
#include <wx/log.h>
#include <wx/timer.h>

#include "widgets/FileHistory.h"
#include "ondemand/ODTaskThread.h"
#include "Experimental.h"

class IPCServ;
class Importer;
class CommandHandler;
class AppCommandEvent;
class AudacityLogger;

void SaveWindowSize();

void QuitAudacity(bool bForce);
void QuitAudacity();

extern bool gIsQuitting;

// Asynchronous open
DECLARE_EXPORTED_EVENT_TYPE(AUDACITY_DLL_API, EVT_OPEN_AUDIO_FILE, -1);

// Keyboard capture support
DECLARE_EXPORTED_EVENT_TYPE(AUDACITY_DLL_API, EVT_CAPTURE_KEYBOARD, -1);
DECLARE_EXPORTED_EVENT_TYPE(AUDACITY_DLL_API, EVT_RELEASE_KEYBOARD, -1);
DECLARE_EXPORTED_EVENT_TYPE(AUDACITY_DLL_API, EVT_CAPTURE_KEY, -1);

// Flags used in command handling.

// These flags represent the majority of the states that affect
// whether or not items in menus are enabled or disabled.
enum
{
   AlwaysEnabledFlag      = 0x00000000,

   AudioIONotBusyFlag     = 0x00000001,
   TimeSelectedFlag       = 0x00000002, // This is equivalent to check if there is a valid selection, so it's used for Zoom to Selection too
   TracksSelectedFlag     = 0x00000004,
   TracksExistFlag        = 0x00000008,
   LabelTracksExistFlag   = 0x00000010,
   WaveTracksSelectedFlag = 0x00000020,
   ClipboardFlag          = 0x00000040,
   TextClipFlag           = 0x00000040, // Same as Clipboard flag for now.
   UnsavedChangesFlag     = 0x00000080,
   HasLastEffectFlag      = 0x00000100,
   UndoAvailableFlag      = 0x00000200,
   RedoAvailableFlag      = 0x00000400,
   ZoomInAvailableFlag    = 0x00000800,
   ZoomOutAvailableFlag   = 0x00001000,
   StereoRequiredFlag     = 0x00002000,  //lda
   TopDockHasFocus        = 0x00004000,  //lll
   TrackPanelHasFocus     = 0x00008000,  //lll
   BotDockHasFocus        = 0x00010000,  //lll
   LabelsSelectedFlag     = 0x00020000,
   AudioIOBusyFlag        = 0x00040000,  //lll
   PlayRegionLockedFlag   = 0x00080000,  //msmeyer
   PlayRegionNotLockedFlag= 0x00100000,  //msmeyer
   CutCopyAvailableFlag   = 0x00200000,
   WaveTracksExistFlag    = 0x00400000,
   NoteTracksExistFlag    = 0x00800000,  //gsw
   NoteTracksSelectedFlag = 0x01000000,  //gsw
   HaveRecentFiles        = 0x02000000,
   IsNotSyncLockedFlag    = 0x04000000,  //awd
   IsSyncLockedFlag       = 0x08000000,  //awd

   NoFlagsSpecifed        = 0xffffffff
};

class BlockFile;

class AudacityApp:public wxApp {
 public:
   virtual bool OnInit(void);
   virtual int OnExit(void);
   virtual void OnFatalException();

#if defined(__WXGTK__)
   int FilterEvent(wxEvent & event);
#endif

   void InitLang( const wxString & lang );

   // These are currently only used on Mac OS, where it's
   // possible to have a menu bar but no windows open.  It doesn't
   // hurt any other platforms, though.
   void OnMenuAbout(wxCommandEvent & event);
   void OnMenuNew(wxCommandEvent & event);
   void OnMenuOpen(wxCommandEvent & event);
   void OnMenuPreferences(wxCommandEvent & event);
   void OnMenuExit(wxCommandEvent & event);

   void OnEndSession(wxCloseEvent & event);

   void OnKeyDown(wxKeyEvent & event);
   void OnChar(wxKeyEvent & event);
   void OnKeyUp(wxKeyEvent & event);

   void OnCaptureKeyboard(wxCommandEvent & event);
   void OnReleaseKeyboard(wxCommandEvent & event);

   // Most Recently Used File support (for all platforms).
   void OnMRUClear(wxCommandEvent &event);
   void OnMRUFile(wxCommandEvent &event);
   // Backend for above - returns true for success, false for failure
   bool MRUOpen(wxString fileName);

   void OnReceiveCommand(AppCommandEvent &event);

   void OnTimer(wxTimerEvent & event);

   /** \brief Mark playback as having missing aliased blockfiles
     *
     * Playback will continue, but the missing files will be silenced
     * ShouldShowMissingAliasedFileWarning can be called to determine
     * if the user should be notified
     */
   void MarkAliasedFilesMissingWarning(BlockFile *b);

   /** \brief Changes the behavior of missing aliased blockfiles warnings
     */
   void SetMissingAliasedFileWarningShouldShow(bool b);

   /** \brief Returns true if the user should be notified of missing alias warnings
     */
   bool ShouldShowMissingAliasedFileWarning();

   #ifdef __WXMAC__
    // In response to Apple Events
    virtual void MacOpenFile(const wxString &fileName) ;
    virtual void MacPrintFile(const wxString &fileName) ;
    virtual void MacNewFile() ;
   #endif

   #if defined(__WXMSW__) && !defined(__WXUNIVERSAL__) && !defined(__CYGWIN__)
    void AssociateFileTypes();
   #endif

   /** \brief A list of directories that should be searched for Audacity files
    * (plug-ins, help files, etc.).
    *
    * On Unix this will include the directory Audacity was installed into,
    * plus the current user's .audacity-data/Plug-Ins directory.  Additional
    * directories can be specified using the AUDACITY_PATH environment
    * variable.  On Windows or Mac OS, this will include the directory
    * which contains the Audacity program. */
   wxArrayString audacityPathList;

   /** \brief Default temp directory */
   wxString defaultTempDir;

   // Useful functions for working with search paths
   static void AddUniquePathToPathList(wxString path,
                                       wxArrayString &pathList);
   static void AddMultiPathsToPathList(wxString multiPathString,
                                       wxArrayString &pathList);
   static void FindFilesInPathList(const wxString & pattern,
                                   const wxArrayString & pathList,
                                   wxArrayString &results,
                                   int flags = wxDIR_FILES);

   FileHistory *GetRecentFiles() {return mRecentFiles;}
   void AddFileToHistory(const wxString & name);
   bool GetWindowRectAlreadySaved()const {return mWindowRectAlreadySaved;}
   void SetWindowRectAlreadySaved(bool alreadySaved) {mWindowRectAlreadySaved = alreadySaved;}

   AudacityLogger *GetLogger();

   Importer *mImporter;

#if defined(__WXGTK__)
   /** \brief This flag is set true when in a keyboard event handler.
    * Used to work around a hang issue with ibus (bug 154) */
   bool inKbdHandler;
#endif

 private:
   CommandHandler *mCmdHandler;
   FileHistory *mRecentFiles;

   wxLocale *mLocale;

   wxSingleInstanceChecker *mChecker;

   wxTimer *mTimer;

   bool                 m_aliasMissingWarningShouldShow;
   BlockFile           *m_LastMissingBlockFile;

   ODLock               m_LastMissingBlockFileLock;

   void InitCommandHandler();
   void DeInitCommandHandler();

   bool InitTempDir();
   bool CreateSingleInstanceChecker(wxString dir);

   /* utility method for printing the command line help message */
   void PrintCommandLineHelp(void);

   bool mWindowRectAlreadySaved;

#if defined(__WXMSW__)
   IPCServ *mIPCServ;
#endif
 public:
    DECLARE_EVENT_TABLE()
};

extern AudacityApp & wxGetApp();

#endif

#define MAX_AUDIO (1. - 1./(1<<15))
#define JUST_BELOW_MAX_AUDIO (1. - 1./(1<<14))
