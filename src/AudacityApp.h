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
#include "audacity/Types.h"

#include "Experimental.h"

#include <wx/app.h> // to inherit
#include <wx/dir.h> // for wxDIR_FILES
#include <wx/timer.h> // member variable

#if defined(EXPERIMENTAL_CRASH_REPORT)
#include <wx/debugrpt.h> // for wxDebugReport::Context
#endif

#include "MemoryX.h"

class wxSingleInstanceChecker;
class wxSocketEvent;
class wxSocketServer;

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

class AliasBlockFile;

class AudacityApp final : public wxApp {
 public:
   AudacityApp();
   ~AudacityApp();
   bool OnInit(void) override;
   int OnExit(void) override;
   void OnFatalException() override;
   bool OnExceptionInMainLoop() override;

   int FilterEvent(wxEvent & event) override;

   // If no input language given, defaults first to choice in preferences, then
   // to system language.
   // Returns the language actually used which is not lang if lang cannot be found.
   wxString InitLang( wxString lang = {} );

   // If no input language given, defaults to system language.
   // Returns the language actually used which is not lang if lang cannot be found.
   wxString SetLang( const wxString & lang );

   // Returns the last language code that was set
   wxString GetLang() const;

   // These are currently only used on Mac OS, where it's
   // possible to have a menu bar but no windows open.  It doesn't
   // hurt any other platforms, though.
   void OnMenuAbout(wxCommandEvent & event);
   void OnMenuNew(wxCommandEvent & event);
   void OnMenuOpen(wxCommandEvent & event);
   void OnMenuPreferences(wxCommandEvent & event);
   void OnMenuExit(wxCommandEvent & event);

   void OnQueryEndSession(wxCloseEvent & event);
   void OnEndSession(wxCloseEvent & event);

   // Most Recently Used File support (for all platforms).
   void OnMRUClear(wxCommandEvent &event);
   void OnMRUFile(wxCommandEvent &event);
   // Backend for above - returns true for success, false for failure
   bool MRUOpen(const FilePath &fileName);
   // A wrapper of the above that does not throw
   bool SafeMRUOpen(const wxString &fileName);

   void OnReceiveCommand(AppCommandEvent &event);

   void OnKeyDown(wxKeyEvent &event);

   void OnTimer(wxTimerEvent & event);

   // IPC communication
   void OnServerEvent(wxSocketEvent & evt);
   void OnSocketEvent(wxSocketEvent & evt);

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
   FilePaths audacityPathList;

   // Useful functions for working with search paths
   static void AddUniquePathToPathList(const FilePath &path,
                                       FilePaths &pathList);
   static void AddMultiPathsToPathList(const wxString &multiPathString,
                                       FilePaths &pathList);
   static void FindFilesInPathList(const wxString & pattern,
                                   const FilePaths & pathList,
                                   FilePaths &results,
                                   int flags = wxDIR_FILES);

   bool GetWindowRectAlreadySaved()const {return mWindowRectAlreadySaved;}
   void SetWindowRectAlreadySaved(bool alreadySaved) {mWindowRectAlreadySaved = alreadySaved;}

   AudacityLogger *GetLogger();

#if defined(EXPERIMENTAL_CRASH_REPORT)
   void GenerateCrashReport(wxDebugReport::Context ctx);
#endif

#ifdef __WXMAC__

   void MacActivateApp();

#endif

   // Set and Get values of the version major/minor/micro keys in audacity.cfg when Audacity first opens
   void SetVersionKeysInit( int major, int minor, int micro)
      { mVersionMajorKeyInit = major; mVersionMinorKeyInit = minor; mVersionMicroKeyInit = micro;}
   void GetVersionKeysInit( int& major, int& minor, int& micro) const
      { major = mVersionMajorKeyInit; minor = mVersionMinorKeyInit; micro = mVersionMicroKeyInit;}


 private:
   std::unique_ptr<CommandHandler> mCmdHandler;

   std::unique_ptr<wxLocale> mLocale;

   std::unique_ptr<wxSingleInstanceChecker> mChecker;

   wxTimer mTimer;

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

   // values of the version major/minor/micro keys in audacity.cfg when Audacity first opens
   int mVersionMajorKeyInit{};
   int mVersionMinorKeyInit{};
   int mVersionMicroKeyInit{};

 public:
    DECLARE_EVENT_TABLE()
};

extern AudacityApp & wxGetApp();

#endif
