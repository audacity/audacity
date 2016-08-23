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

#include "MemoryX.h"
#include <wx/app.h>
#include <wx/cmdline.h>
#include <wx/dir.h>
#include <wx/event.h>
#include <wx/docview.h>
#include <wx/intl.h>
#include <wx/snglinst.h>
#include <wx/log.h>
#include <wx/socket.h>
#include <wx/timer.h>

#include "widgets/FileHistory.h"
#include "ondemand/ODTaskThread.h"
#include "Experimental.h"

#if defined(EXPERIMENTAL_CRASH_REPORT)
#include <wx/debugrpt.h>
#endif

class IPCServ;
class Importer;
class CommandHandler;
class AppCommandEvent;
class AudacityLogger;
class AudacityProject;

void SaveWindowSize();

void QuitAudacity(bool bForce);
void QuitAudacity();

extern bool gIsQuitting;

// Asynchronous open
DECLARE_EXPORTED_EVENT_TYPE(AUDACITY_DLL_API, EVT_OPEN_AUDIO_FILE, -1);

// Flags used in command handling.

// These flags represent the majority of the states that affect
// whether or not items in menus are enabled or disabled.
enum CommandFlag : unsigned long long
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
   IsRealtimeNotActiveFlag= 0x10000000,  //lll
   CaptureNotBusyFlag     = 0x20000000,
   CanStopAudioStreamFlag = 0x40000000,
   RulerHasFocus          = 0x80000000ULL, // prl
   NotMinimizedFlag      = 0x100000000ULL, // prl
   PausedFlag            = 0x200000000ULL, // jkc
   NotPausedFlag         = 0x400000000ULL, // jkc
   HasWaveDataFlag       = 0x800000000ULL, // jkc

   NoFlagsSpecifed        = ~0ULL
};

// Prevent accidental misuse with narrower types

bool operator == (CommandFlag, unsigned long) PROHIBITED;
bool operator == (CommandFlag, long) PROHIBITED;
bool operator == (unsigned long, CommandFlag) PROHIBITED;
bool operator == (long, CommandFlag) PROHIBITED;

bool operator != (CommandFlag, unsigned long) PROHIBITED;
bool operator != (CommandFlag, long) PROHIBITED;
bool operator != (unsigned long, CommandFlag) PROHIBITED;
bool operator != (long, CommandFlag) PROHIBITED;

CommandFlag operator & (CommandFlag, unsigned long) PROHIBITED;
CommandFlag operator & (CommandFlag, long) PROHIBITED;
CommandFlag operator & (unsigned long, CommandFlag) PROHIBITED;
CommandFlag operator & (long, CommandFlag) PROHIBITED;

CommandFlag operator | (CommandFlag, unsigned long) PROHIBITED;
CommandFlag operator | (CommandFlag, long) PROHIBITED;
CommandFlag operator | (unsigned long, CommandFlag) PROHIBITED;
CommandFlag operator | (long, CommandFlag) PROHIBITED;

CommandFlag operator ^ (CommandFlag, unsigned long) PROHIBITED;
CommandFlag operator ^ (CommandFlag, long) PROHIBITED;
CommandFlag operator ^ (unsigned long, CommandFlag) PROHIBITED;
CommandFlag operator ^ (long, CommandFlag) PROHIBITED;

bool operator == (CommandFlag, unsigned int) PROHIBITED;
bool operator == (CommandFlag, int) PROHIBITED;
bool operator == (unsigned int, CommandFlag) PROHIBITED;
bool operator == (int, CommandFlag) PROHIBITED;

bool operator != (CommandFlag, unsigned int) PROHIBITED;
bool operator != (CommandFlag, int) PROHIBITED;
bool operator != (unsigned int, CommandFlag) PROHIBITED;
bool operator != (int, CommandFlag) PROHIBITED;

CommandFlag operator & (CommandFlag, unsigned int) PROHIBITED;
CommandFlag operator & (CommandFlag, int) PROHIBITED;
CommandFlag operator & (unsigned int, CommandFlag) PROHIBITED;
CommandFlag operator & (int, CommandFlag) PROHIBITED;

CommandFlag operator | (CommandFlag, unsigned int) PROHIBITED;
CommandFlag operator | (CommandFlag, int) PROHIBITED;
CommandFlag operator | (unsigned int, CommandFlag) PROHIBITED;
CommandFlag operator | (int, CommandFlag) PROHIBITED;

CommandFlag operator ^ (CommandFlag, unsigned int) PROHIBITED;
CommandFlag operator ^ (CommandFlag, int) PROHIBITED;
CommandFlag operator ^ (unsigned int, CommandFlag) PROHIBITED;
CommandFlag operator ^ (int, CommandFlag) PROHIBITED;

// Supply the bitwise operations

inline CommandFlag operator ~ (CommandFlag flag)
{
   return static_cast<CommandFlag>( ~ static_cast<unsigned long long> (flag) );
}
inline CommandFlag operator & (CommandFlag lhs, CommandFlag rhs)
{
   return static_cast<CommandFlag> (
      static_cast<unsigned long long>(lhs) & static_cast<unsigned long long>(rhs)
   );
}
inline CommandFlag operator | (CommandFlag lhs, CommandFlag rhs)
{
   return static_cast<CommandFlag> (
      static_cast<unsigned long long>(lhs) | static_cast<unsigned long long>(rhs)
   );
}
inline CommandFlag & operator |= (CommandFlag &lhs, CommandFlag rhs)
{
   lhs = lhs | rhs;
   return lhs;
}

using CommandMask = CommandFlag;

class BlockFile;
class AliasBlockFile;

class AudacityApp final : public wxApp {
 public:
   AudacityApp();
   ~AudacityApp();
   bool OnInit(void) override;
   int OnExit(void) override;
   void OnFatalException() override;

   int FilterEvent(wxEvent & event);

   // Returns the language actually used which is not lang if lang cannot be found.
   wxString InitLang( const wxString & lang );

   // These are currently only used on Mac OS, where it's
   // possible to have a menu bar but no windows open.  It doesn't
   // hurt any other platforms, though.
   void OnMenuAbout(wxCommandEvent & event);
   void OnMenuNew(wxCommandEvent & event);
   void OnMenuOpen(wxCommandEvent & event);
   void OnMenuPreferences(wxCommandEvent & event);
   void OnMenuExit(wxCommandEvent & event);

   void OnEndSession(wxCloseEvent & event);

   // Most Recently Used File support (for all platforms).
   void OnMRUClear(wxCommandEvent &event);
   void OnMRUFile(wxCommandEvent &event);
   // Backend for above - returns true for success, false for failure
   bool MRUOpen(const wxString &fileName);

   void OnReceiveCommand(AppCommandEvent &event);

   void OnKeyDown(wxKeyEvent &event);

   void OnTimer(wxTimerEvent & event);

   // IPC communication
   void OnServerEvent(wxSocketEvent & evt);
   void OnSocketEvent(wxSocketEvent & evt);

   /** \brief Mark playback as having missing aliased blockfiles
     *
     * Playback will continue, but the missing files will be silenced
     * ShouldShowMissingAliasedFileWarning can be called to determine
     * if the user should be notified
     */
   void MarkAliasedFilesMissingWarning(const AliasBlockFile *b);

   /** \brief Changes the behavior of missing aliased blockfiles warnings
     */
   void SetMissingAliasedFileWarningShouldShow(bool b);

   /** \brief Returns true if the user should be notified of missing alias warnings
     */
   bool ShouldShowMissingAliasedFileWarning();

   #ifdef __WXMAC__
    // In response to Apple Events
    void MacOpenFile(const wxString &fileName)  override;
    void MacPrintFile(const wxString &fileName)  override;
    void MacNewFile()  override;
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
   static void AddUniquePathToPathList(const wxString &path,
                                       wxArrayString &pathList);
   static void AddMultiPathsToPathList(const wxString &multiPathString,
                                       wxArrayString &pathList);
   static void FindFilesInPathList(const wxString & pattern,
                                   const wxArrayString & pathList,
                                   wxArrayString &results,
                                   int flags = wxDIR_FILES);
   static bool IsTempDirectoryNameOK( const wxString & Name );

   FileHistory *GetRecentFiles() {return mRecentFiles.get();}
   void AddFileToHistory(const wxString & name);
   bool GetWindowRectAlreadySaved()const {return mWindowRectAlreadySaved;}
   void SetWindowRectAlreadySaved(bool alreadySaved) {mWindowRectAlreadySaved = alreadySaved;}

   AudacityLogger *GetLogger();

#if defined(EXPERIMENTAL_CRASH_REPORT)
   void GenerateCrashReport(wxDebugReport::Context ctx);
#endif

 private:
   std::unique_ptr<CommandHandler> mCmdHandler;
   std::unique_ptr<FileHistory> mRecentFiles;

   std::unique_ptr<wxLocale> mLocale;

   std::unique_ptr<wxSingleInstanceChecker> mChecker;

   wxTimer mTimer;

   bool                 m_aliasMissingWarningShouldShow;
   std::weak_ptr< AudacityProject > m_LastMissingBlockFileProject;
   wxString             m_LastMissingBlockFilePath;

   ODLock               m_LastMissingBlockFileLock;

   void InitCommandHandler();

   bool InitTempDir();
   bool CreateSingleInstanceChecker(const wxString &dir);

   std::unique_ptr<wxCmdLineParser> ParseCommandLine();

   bool mWindowRectAlreadySaved;

#if defined(__WXMSW__)
   std::unique_ptr<IPCServ> mIPCServ;
#else
   std::unique_ptr<wxSocketServer> mIPCServ;
#endif

 public:
    DECLARE_EVENT_TABLE()
};

extern AudacityApp & wxGetApp();

#endif

#define MAX_AUDIO (1. - 1./(1<<15))
#define JUST_BELOW_MAX_AUDIO (1. - 1./(1<<14))
