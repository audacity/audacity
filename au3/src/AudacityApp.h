/**********************************************************************

  Audacity: A Digital Audio Editor

  AudacityApp.h

  Dominic Mazzoni

  This is the main source file for Audacity which handles
  initialization and termination by subclassing wxApp.

**********************************************************************/

#ifndef __AUDACITY_APP__
#define __AUDACITY_APP__

#include "Identifier.h"
#include "Observer.h"
#include "Theme.h"
#include "AppEvents.h"

#include <wx/app.h> // to inherit
#include <wx/splash.h> // member variable
#include <wx/timer.h> // member variable

#include <memory>

class wxSingleInstanceChecker;
class wxSocketEvent;
class wxSocketServer;

class IPCServ;
class Importer;
class CommandHandler;
class AppCommandEvent;
class AudacityProject;

class AudacityApp final : public wxApp, private AppEvents::ProviderBase
{
public:
    AudacityApp();
    ~AudacityApp();

    bool Initialize(int& argc, wxChar** argv) override;
#ifdef __WXMAC__
    bool OSXIsGUIApplication() override;
#endif

    void CleanUp() override;
    bool OnInit() override;
    bool InitPart2();
    int OnRun() override;
    int OnExit(void) override;
    void OnFatalException() override;
    bool OnExceptionInMainLoop() override;

    void OnIdle(wxIdleEvent&);

    // These are currently only used on Mac OS, where it's
    // possible to have a menu bar but no windows open.  It doesn't
    // hurt any other platforms, though.
    void OnMenuAbout(wxCommandEvent& event);
    void OnMenuNew(wxCommandEvent& event);
    void OnMenuOpen(wxCommandEvent& event);
    void OnMenuPreferences(wxCommandEvent& event);
    void OnMenuExit(wxCommandEvent& event);

    void OnQueryEndSession(wxCloseEvent& event);
    void OnEndSession(wxCloseEvent& event);

    // Most Recently Used File support (for all platforms).
    void OnMRUClear(wxCommandEvent& event);
    void OnMRUFile(wxCommandEvent& event);
    // Backend for above - returns true for success, false for failure
    bool MRUOpen(const FilePath& fileName);
    // A wrapper of the above that does not throw
    bool SafeMRUOpen(const wxString& fileName);

    void OnReceiveCommand(AppCommandEvent& event);

    void OnKeyDown(wxKeyEvent& event);

    void OnTimer(wxTimerEvent& event);

    // IPC communication
    void OnServerEvent(wxSocketEvent& evt);
    void OnSocketEvent(wxSocketEvent& evt);

   #ifdef __WXMAC__
    // In response to Apple Events
    void MacOpenFile(const wxString& fileName)  override;
    void MacPrintFile(const wxString& fileName)  override;
    void MacNewFile()  override;
   #ifdef HAS_CUSTOM_URL_HANDLING
    void MacOpenURL(const wxString& url) override;
   #endif
   #endif

   #if defined(__WXMSW__) && !defined(__WXUNIVERSAL__) && !defined(__CYGWIN__)
    void AssociateFileTypes();
   #endif

    static void OnThemeChange(struct ThemeChangeMessage);

#ifdef __WXMAC__
    void MacActivateApp();
    void MacFinishLaunching();
#endif

private:
    void OnInit0();
    Observer::Subscription mThemeChangeSubscription;

    std::unique_ptr<CommandHandler> mCmdHandler;

    std::unique_ptr<wxSingleInstanceChecker> mChecker;

    wxTimer mTimer;
    wxTimer mSplashTimer;
    std::unique_ptr<wxSplashScreen> mSplashScreen;

    void InitCommandHandler();

    bool InitTempDir();
    bool CreateSingleInstanceChecker(const wxString& dir);
    void ShowSplashScreen();
    void HideSplashScreen(bool fadeOut=true);

    std::unique_ptr<wxCmdLineParser> ParseCommandLine();

#if defined(__WXMSW__)
    std::unique_ptr<IPCServ> mIPCServ;
#else
    std::unique_ptr<wxSocketServer> mIPCServ;
#endif

public:
    DECLARE_EVENT_TABLE()
};

extern AudacityApp& wxGetApp();

#endif
