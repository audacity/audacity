/**********************************************************************

  Audacity: A Digital Audio Editor

  @file PathList.cpp

  Paul Licameli split from AudacityApp.cpp

**********************************************************************/

#include "PathList.h"

#include "FileNames.h"
#include "TempDirectory.h"
#include <wx/stdpaths.h>
#include <wx/utils.h>



#if 0
// This may be used to debug memory leaks.
// See: Visual Leak Detector @ http://vld.codeplex.com/
#include <vld.h>

#include <wx/setup.h> // for wxUSE_* macros
#include <wx/wxcrtvararg.h>
#include <wx/defs.h>
#include <wx/evtloop.h>
#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/docview.h>
#include <wx/event.h>
#include <wx/ipc.h>
#include <wx/window.h>
#include <wx/intl.h>
#include <wx/menu.h>
#include <wx/snglinst.h>
#include <wx/splash.h>
#include <wx/stdpaths.h>
#include <wx/sysopt.h>
#include <wx/fontmap.h>

#include <wx/fs_zip.h>
#include <wx/image.h>

#include <wx/dir.h>
#include <wx/file.h>
#include <wx/filename.h>

#ifdef __WXGTK__
#include <unistd.h>
#ifdef HAVE_GTK
#include <gtk/gtk.h>
#endif
#endif

// chmod, lstat, geteuid
#ifdef __UNIX__
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <stdio.h>
#endif

#if defined(__WXMSW__)
#include <wx/msw/registry.h> // for wxRegKey
#endif

#include "AudacityLogger.h"
#include "AboutDialog.h"
#include "ActiveProject.h"
#include "AColor.h"
#include "AudacityFileConfig.h"
#include "AudioIO.h"
#include "Benchmark.h"
#include "Clipboard.h"
#include "CrashReport.h" // for HAS_CRASH_REPORT
#include "commands/CommandHandler.h"
#include "commands/AppCommandEvent.h"
#include "widgets/ASlider.h"
#include "FFmpeg.h"
#include "Journal.h"
//#include "LangChoice.h"
#include "Languages.h"
#include "Menus.h"
#include "PluginManager.h"
#include "Project.h"
#include "ProjectAudioIO.h"
#include "ProjectAudioManager.h"
#include "ProjectFileIO.h"
#include "ProjectFileManager.h"
#include "ProjectHistory.h"
#include "ProjectManager.h"
#include "ProjectSettings.h"
#include "ProjectWindow.h"
#include "ProjectWindows.h"
#include "Screenshot.h"
#include "Sequence.h"
#include "SelectFile.h"
#include "TempDirectory.h"
#include "LoadThemeResources.h"
#include "Track.h"
#include "prefs/PrefsDialog.h"
#include "Theme.h"
#include "PlatformCompatibility.h"
#include "AutoRecoveryDialog.h"
#include "SplashDialog.h"
#include "FFT.h"
#include "widgets/AudacityMessageBox.h"
#include "prefs/DirectoriesPrefs.h"
#include "prefs/GUISettings.h"
#include "tracks/ui/Scrubbing.h"
#include "FileConfig.h"
#include "widgets/FileHistory.h"
#include "update/UpdateManager.h"
#include "widgets/wxWidgetsBasicUI.h"
#include "LogWindow.h"

#ifdef HAS_NETWORKING
#include "NetworkManager.h"
#endif

#ifdef EXPERIMENTAL_EASY_CHANGE_KEY_BINDINGS
#include "prefs/KeyConfigPrefs.h"
#endif

//temporarily commented out till it is added to all projects
//#include "Profiler.h"

#include "ModuleManager.h"

#include "import/Import.h"

#if defined(USE_BREAKPAD)
#include "BreakpadConfigurer.h"
#endif

#ifdef EXPERIMENTAL_SCOREALIGN
#include "effects/ScoreAlignDialog.h"
#endif

#if 0
#ifdef _DEBUG
    #ifdef _MSC_VER
        #undef THIS_FILE
        static char*THIS_FILE= __FILE__;
        #define new new(_NORMAL_BLOCK, THIS_FILE, __LINE__)
    #endif
#endif
#endif

// DA: Logo for Splash Screen
#ifdef EXPERIMENTAL_DA
#include "../images/DarkAudacityLogoWithName.xpm"
#else
#include "../images/AudacityLogoWithName.xpm"
#endif

#include <thread>


////////////////////////////////////////////////////////////
/// Custom events
////////////////////////////////////////////////////////////

#if 0
#ifdef __WXGTK__
static void wxOnAssert(const wxChar *fileName, int lineNumber, const wxChar *msg)
{
   if (msg)
      wxPrintf("ASSERTION FAILED: %s\n%s: %d\n", (const char *)wxString(msg).mb_str(), (const char *)wxString(fileName).mb_str(), lineNumber);
   else
      wxPrintf("ASSERTION FAILED!\n%s: %d\n", (const char *)wxString(fileName).mb_str(), lineNumber);

   // Force core dump
   int *i = 0;
   if (*i)
      exit(1);

   exit(0);
}
#endif
#endif

namespace {

void PopulatePreferences()
{
   bool resetPrefs = false;
   wxString langCode = gPrefs->Read(wxT("/Locale/Language"), wxEmptyString);
   bool writeLang = false;

   const wxFileName fn(
      FileNames::ResourcesDir(), 
      wxT("FirstTime.ini"));
   if (fn.FileExists())   // it will exist if the (win) installer put it there
   {
      const wxString fullPath{fn.GetFullPath()};

      auto pIni =
         AudacityFileConfig::Create({}, {}, fullPath, {},
            wxCONFIG_USE_LOCAL_FILE);
      auto &ini = *pIni;

      wxString lang;
      if (ini.Read(wxT("/FromInno/Language"), &lang) && !lang.empty())
      {
         // Only change "langCode" if the language was actually specified in the ini file.
         langCode = lang;
         writeLang = true;

         // Inno Setup doesn't allow special characters in the Name values, so "0" is used
         // to represent the "@" character.
         langCode.Replace(wxT("0"), wxT("@"));
      }

      ini.Read(wxT("/FromInno/ResetPrefs"), &resetPrefs, false);

      bool gone = wxRemoveFile(fullPath);  // remove FirstTime.ini
      if (!gone)
      {
         AudacityMessageBox(
            XO("Failed to remove %s").Format(fullPath),
            XO("Failed!"));
      }
   }

   // Use the system default language if one wasn't specified or if the user selected System.
   if (langCode.empty())
      langCode =
         Languages::GetSystemLanguageCode(FileNames::AudacityPathList());

   langCode = GUISettings::SetLang( langCode );

   // User requested that the preferences be completely reset
   if (resetPrefs)
   {
      // pop up a dialogue
      auto prompt = XO(
"Reset Preferences?\n\nThis is a one-time question, after an 'install' where you asked to have the Preferences reset.");
      int action = AudacityMessageBox(
         prompt,
         XO("Reset Audacity Preferences"),
         wxYES_NO, NULL);
      if (action == wxYES)   // reset
      {
         ResetPreferences();
         writeLang = true;
      }
   }

   // Save the specified language
   if (writeLang)
   {
      gPrefs->Write(wxT("/Locale/Language"), langCode);
   }

   // In AUdacity 2.1.0 support for the legacy 1.2.x preferences (depreciated since Audacity
   // 1.3.1) is dropped. As a result we can drop the import flag
   // first time this version of Audacity is run we try to migrate
   // old preferences.
   bool newPrefsInitialized = false;
   gPrefs->Read(wxT("/NewPrefsInitialized"), &newPrefsInitialized, false);
   if (newPrefsInitialized) {
      gPrefs->DeleteEntry(wxT("/NewPrefsInitialized"), true);  // take group as well if empty
   }

   // record the Prefs version for future checking (this has not been used for a very
   // long time).
   gPrefs->Write(wxT("/PrefsVersion"), wxString(wxT(AUDACITY_PREFS_VERSION_STRING)));

   // Check if some prefs updates need to happen based on audacity version.
   // Unfortunately we can't use the PrefsVersion prefs key because that resets things.
   // In the future we may want to integrate that better.
   // these are done on a case-by-case basis for now so they must be backwards compatible
   // (meaning the changes won't mess audacity up if the user goes back to an earlier version)
   int vMajor = gPrefs->Read(wxT("/Version/Major"), (long) 0);
   int vMinor = gPrefs->Read(wxT("/Version/Minor"), (long) 0);
   int vMicro = gPrefs->Read(wxT("/Version/Micro"), (long) 0);

   gPrefs->SetVersionKeysInit(vMajor, vMinor, vMicro);   // make a note of these initial values
                                                            // for use by ToolManager::ReadConfig()

   // These integer version keys were introduced april 4 2011 for 1.3.13
   // The device toolbar needs to be enabled due to removal of source selection features in
   // the mixer toolbar.
   if ((vMajor < 1) ||
       (vMajor == 1 && vMinor < 3) ||
       (vMajor == 1 && vMinor == 3 && vMicro < 13)) {


      // Do a full reset of the Device Toolbar to get it on the screen.
      if (gPrefs->Exists(wxT("/GUI/ToolBars/Device")))
         gPrefs->DeleteGroup(wxT("/GUI/ToolBars/Device"));

      // We keep the mixer toolbar prefs (shown/not shown)
      // the width of the mixer toolbar may have shrunk, the prefs will keep the larger value
      // if the user had a device that had more than one source.
      if (gPrefs->Exists(wxT("/GUI/ToolBars/Mixer"))) {
         // Use the default width
         gPrefs->Write(wxT("/GUI/ToolBars/Mixer/W"), -1);
      }
   }

   // In 2.1.0, the Meter toolbar was split and lengthened, but strange arrangements happen
   // if upgrading due to the extra length.  So, if a user is upgrading, use the pre-2.1.0
   // lengths, but still use the NEW split versions.
   if (gPrefs->Exists(wxT("/GUI/ToolBars/Meter")) &&
      !gPrefs->Exists(wxT("/GUI/ToolBars/CombinedMeter"))) {

      // Read in all of the existing values
      long dock, order, show, x, y, w, h;
      gPrefs->Read(wxT("/GUI/ToolBars/Meter/Dock"), &dock, -1);
      gPrefs->Read(wxT("/GUI/ToolBars/Meter/Order"), &order, -1);
      gPrefs->Read(wxT("/GUI/ToolBars/Meter/Show"), &show, -1);
      gPrefs->Read(wxT("/GUI/ToolBars/Meter/X"), &x, -1);
      gPrefs->Read(wxT("/GUI/ToolBars/Meter/Y"), &y, -1);
      gPrefs->Read(wxT("/GUI/ToolBars/Meter/W"), &w, -1);
      gPrefs->Read(wxT("/GUI/ToolBars/Meter/H"), &h, -1);

      // "Order" must be adjusted since we're inserting two NEW toolbars
      if (dock > 0) {
         wxString oldPath = gPrefs->GetPath();
         gPrefs->SetPath(wxT("/GUI/ToolBars"));

         wxString bar;
         long ndx = 0;
         bool cont = gPrefs->GetFirstGroup(bar, ndx);
         while (cont) {
            long o;
            if (gPrefs->Read(bar + wxT("/Order"), &o) && o >= order) {
               gPrefs->Write(bar + wxT("/Order"), o + 2);
            }
            cont = gPrefs->GetNextGroup(bar, ndx);
         }
         gPrefs->SetPath(oldPath);

         // And override the height
         h = 27;
      }

      // Write the split meter bar values
      gPrefs->Write(wxT("/GUI/ToolBars/RecordMeter/Dock"), dock);
      gPrefs->Write(wxT("/GUI/ToolBars/RecordMeter/Order"), order);
      gPrefs->Write(wxT("/GUI/ToolBars/RecordMeter/Show"), show);
      gPrefs->Write(wxT("/GUI/ToolBars/RecordMeter/X"), -1);
      gPrefs->Write(wxT("/GUI/ToolBars/RecordMeter/Y"), -1);
      gPrefs->Write(wxT("/GUI/ToolBars/RecordMeter/W"), w);
      gPrefs->Write(wxT("/GUI/ToolBars/RecordMeter/H"), h);
      gPrefs->Write(wxT("/GUI/ToolBars/PlayMeter/Dock"), dock);
      gPrefs->Write(wxT("/GUI/ToolBars/PlayMeter/Order"), order + 1);
      gPrefs->Write(wxT("/GUI/ToolBars/PlayMeter/Show"), show);
      gPrefs->Write(wxT("/GUI/ToolBars/PlayMeter/X"), -1);
      gPrefs->Write(wxT("/GUI/ToolBars/PlayMeter/Y"), -1);
      gPrefs->Write(wxT("/GUI/ToolBars/PlayMeter/W"), w);
      gPrefs->Write(wxT("/GUI/ToolBars/PlayMeter/H"), h);

      // And hide the old combined meter bar
      gPrefs->Write(wxT("/GUI/ToolBars/Meter/Dock"), -1);
   }

   // Upgrading pre 2.2.0 configs we assume extended set of defaults.
   if ((0<vMajor && vMajor < 2) ||
       (vMajor == 2 && vMinor < 2))
   {
      gPrefs->Write(wxT("/GUI/Shortcuts/FullDefaults"),1);
   }

   // Upgrading pre 2.4.0 configs, the selection toolbar is now split.
   if ((0<vMajor && vMajor < 2) ||
       (vMajor == 2 && vMinor < 4))
   {
      gPrefs->Write(wxT("/GUI/Toolbars/Selection/W"),"");
      gPrefs->Write(wxT("/GUI/Toolbars/SpectralSelection/W"),"");
      gPrefs->Write(wxT("/GUI/Toolbars/Time/X"),-1);
      gPrefs->Write(wxT("/GUI/Toolbars/Time/Y"),-1);
      gPrefs->Write(wxT("/GUI/Toolbars/Time/H"),55);
      gPrefs->Write(wxT("/GUI/Toolbars/Time/W"),251);
      gPrefs->Write(wxT("/GUI/Toolbars/Time/DockV2"),2);
      gPrefs->Write(wxT("/GUI/Toolbars/Time/Dock"),2);
      gPrefs->Write(wxT("/GUI/Toolbars/Time/Path"),"0,1");
      gPrefs->Write(wxT("/GUI/Toolbars/Time/Show"),1);
   }

   if (std::pair{ vMajor, vMinor } < std::pair{ 3, 1 } ) {
      // Reset the control toolbar
      gPrefs->Write(wxT("/GUI/Toolbars/Control/W"), -1);
   }

   // write out the version numbers to the prefs file for future checking
   gPrefs->Write(wxT("/Version/Major"), AUDACITY_VERSION);
   gPrefs->Write(wxT("/Version/Minor"), AUDACITY_RELEASE);
   gPrefs->Write(wxT("/Version/Micro"), AUDACITY_REVISION);

   gPrefs->Flush();
}

#if defined(USE_BREAKPAD)
void InitBreakpad()
{
    wxFileName databasePath;
    databasePath.SetPath(wxStandardPaths::Get().GetUserLocalDataDir());
    databasePath.AppendDir("crashreports");
    databasePath.Mkdir(wxS_DIR_DEFAULT, wxPATH_MKDIR_FULL);
    
    if(databasePath.DirExists())
    {   
        const auto sentryRelease = wxString::Format(
           "audacity@%d.%d.%d", AUDACITY_VERSION, AUDACITY_RELEASE, AUDACITY_REVISION
        );
        BreakpadConfigurer configurer;
        configurer.SetDatabasePathUTF8(databasePath.GetPath().ToUTF8().data())
            .SetSenderPathUTF8(wxFileName(wxStandardPaths::Get().GetExecutablePath()).GetPath().ToUTF8().data())
    #if defined(CRASH_REPORT_URL)
            .SetReportURL(CRASH_REPORT_URL)
    #endif
            .SetParameters({
                { "version", wxString(AUDACITY_VERSION_STRING).ToUTF8().data() },
                { "sentry[release]",  sentryRelease.ToUTF8().data() }
            })
            .Start();
    }
}
#endif
}

static bool gInited = false;
static bool gIsQuitting = false;

static bool CloseAllProjects( bool force )
{
   ProjectManager::SetClosingAll(true);
   auto cleanup = finally([]{ ProjectManager::SetClosingAll(false); });
   while (AllProjects{}.size())
   {
      // Closing the project has global side-effect
      // of deletion from gAudacityProjects
      if ( force )
      {
         GetProjectFrame( **AllProjects{}.begin() ).Close(true);
      }
      else
      {
         if (! GetProjectFrame( **AllProjects{}.begin() ).Close())
            return false;
      }
   }
   return true;
}

static void QuitAudacity(bool bForce)
{
   // guard against recursion
   if (gIsQuitting)
      return;

   gIsQuitting = true;

   wxTheApp->SetExitOnFrameDelete(true);

   // Try to close each open window.  If the user hits Cancel
   // in a Save Changes dialog, don't continue.
   // BG: unless force is true

   // BG: Are there any projects open?
   //-   if (!AllProjects{}.empty())
/*start+*/
   if (AllProjects{}.empty())
   {
#ifdef __WXMAC__
      Clipboard::Get().Clear();
#endif
   }
   else
/*end+*/
   {
      if (AllProjects{}.size())
         // PRL:  Always did at least once before close might be vetoed
         // though I don't know why that is important
         ProjectManager::SaveWindowSize();
      bool closedAll = CloseAllProjects( bForce );
      if ( !closedAll )
      {
         gIsQuitting = false;
         return;
      }
   }

   ModuleManager::Get().Dispatch(AppQuiting);

#ifdef EXPERIMENTAL_SCOREALIGN
   CloseScoreAlignDialog();
#endif
   CloseScreenshotTools();

   // Logger window is always destroyed on macOS,
   // on other platforms - it prevents the runloop
   // termination when exiting is requested
   #if !defined(__WXMAC__)
   LogWindow::Destroy();
   #endif

   //print out profile if we have one by deleting it
   //temporarily commented out till it is added to all projects
   //DELETE Profiler::Instance();

   // Save last log for diagnosis
   auto logger = AudacityLogger::Get();
   if (logger)
   {
      wxFileName logFile(FileNames::DataDir(), wxT("lastlog.txt"));
      logger->SaveLog(logFile.GetFullPath());
   }

   //remove our logger
   std::unique_ptr<wxLog>{ wxLog::SetActiveTarget(NULL) }; // DELETE

   if (bForce)
   {
      wxExit();
   }
}

static void QuitAudacity()
{
   QuitAudacity(false);
}

#if defined(__WXGTK__) && defined(HAVE_GTK)

///////////////////////////////////////////////////////////////////////////////
// Provide the ability to receive notification from the session manager
// when the user is logging out or shutting down.
//
// Most of this was taken from nsNativeAppSupportUnix.cpp from Mozilla.
///////////////////////////////////////////////////////////////////////////////

// TODO: May need updating.  Is this code too obsolete (relying on Gnome2 so's) to be
// worth keeping anymore?
// CB suggests we use libSM directly ref:
// http://www.x.org/archive/X11R7.7/doc/libSM/SMlib.html#The_Save_Yourself_Callback

#include <dlfcn.h>
/* There is a conflict between the type names used in Glib >= 2.21 and those in
 * wxGTK (http://trac.wxwidgets.org/ticket/10883)
 * Happily we can avoid the hack, as we only need some of the headers, not
 * the full GTK headers
 */
#include <glib-object.h>

typedef struct _GnomeProgram GnomeProgram;
typedef struct _GnomeModuleInfo GnomeModuleInfo;
typedef struct _GnomeClient GnomeClient;

typedef enum
{
  GNOME_SAVE_GLOBAL,
  GNOME_SAVE_LOCAL,
  GNOME_SAVE_BOTH
} GnomeSaveStyle;

typedef enum
{
  GNOME_INTERACT_NONE,
  GNOME_INTERACT_ERRORS,
  GNOME_INTERACT_ANY
} GnomeInteractStyle;

typedef enum
{
  GNOME_DIALOG_ERROR,
  GNOME_DIALOG_NORMAL
} GnomeDialogType;

typedef GnomeProgram * (*_gnome_program_init_fn)(const char *,
                                                 const char *,
                                                 const GnomeModuleInfo *,
                                                 int,
                                                 char **,
                                                 const char *,
                                                 ...);
typedef const GnomeModuleInfo * (*_libgnomeui_module_info_get_fn)();
typedef GnomeClient * (*_gnome_master_client_fn)(void);
typedef void (*GnomeInteractFunction)(GnomeClient *,
                                      gint,
                                      GnomeDialogType,
                                      gpointer);
typedef void (*_gnome_client_request_interaction_fn)(GnomeClient *,
                                                     GnomeDialogType,
                                                     GnomeInteractFunction,
                                                     gpointer);
typedef void (*_gnome_interaction_key_return_fn)(gint, gboolean);

static _gnome_client_request_interaction_fn gnome_client_request_interaction;
static _gnome_interaction_key_return_fn gnome_interaction_key_return;

static void interact_cb(GnomeClient * /* client */,
                        gint key,
                        GnomeDialogType /* type */,
                        gpointer /* data */)
{
   wxCloseEvent e(wxEVT_QUERY_END_SESSION, wxID_ANY);
   e.SetEventObject(&wxGetApp());
   e.SetCanVeto(true);

   wxGetApp().ProcessEvent(e);

   gnome_interaction_key_return(key, e.GetVeto());
}

static gboolean save_yourself_cb(GnomeClient *client,
                                 gint /* phase */,
                                 GnomeSaveStyle /* style */,
                                 gboolean shutdown,
                                 GnomeInteractStyle interact,
                                 gboolean /* fast */,
                                 gpointer /* user_data */)
{
   if (!shutdown || interact != GNOME_INTERACT_ANY) {
      return TRUE;
   }

   if (AllProjects{}.empty()) {
      return TRUE;
   }

   gnome_client_request_interaction(client,
                                    GNOME_DIALOG_NORMAL,
                                    interact_cb,
                                    NULL);

   return TRUE;
}

class GnomeShutdown
{
 public:
   GnomeShutdown()
   {
      mArgv[0].reset(strdup("Audacity"));

      mGnomeui = dlopen("libgnomeui-2.so.0", RTLD_NOW);
      if (!mGnomeui) {
         return;
      }

      mGnome = dlopen("libgnome-2.so.0", RTLD_NOW);
      if (!mGnome) {
         return;
      }

      _gnome_program_init_fn gnome_program_init = (_gnome_program_init_fn)
         dlsym(mGnome, "gnome_program_init");
      _libgnomeui_module_info_get_fn libgnomeui_module_info_get = (_libgnomeui_module_info_get_fn)
         dlsym(mGnomeui, "libgnomeui_module_info_get");
      _gnome_master_client_fn gnome_master_client = (_gnome_master_client_fn)
         dlsym(mGnomeui, "gnome_master_client");

      gnome_client_request_interaction = (_gnome_client_request_interaction_fn)
         dlsym(mGnomeui, "gnome_client_request_interaction");
      gnome_interaction_key_return = (_gnome_interaction_key_return_fn)
         dlsym(mGnomeui, "gnome_interaction_key_return");


      if (!gnome_program_init || !libgnomeui_module_info_get) {
         return;
      }

      gnome_program_init(mArgv[0].get(),
                         "1.0",
                         libgnomeui_module_info_get(),
                         1,
                         reinterpret_cast<char**>(mArgv),
                         NULL);

      mClient = gnome_master_client();
      if (mClient == NULL) {
         return;
      }

      g_signal_connect(mClient, "save-yourself", G_CALLBACK(save_yourself_cb), NULL);
   }

   virtual ~GnomeShutdown()
   {
      // Do not dlclose() the libraries here lest you want segfaults...
   }

 private:

   MallocString<> mArgv[1];
   void *mGnomeui;
   void *mGnome;
   GnomeClient *mClient;
};

// This variable exists to call the constructor and
// connect a signal for the 'save-yourself' message.
GnomeShutdown GnomeShutdownInstance;

#endif

// Where drag/drop or "Open With" filenames get stored until
// the timer routine gets around to picking them up.
static wxArrayString ofqueue;

//
// DDE support for opening multiple files with one instance
// of Audacity.
//

#define IPC_APPL wxT("audacity")
#define IPC_TOPIC wxT("System")

class IPCConn final : public wxConnection
{
public:
   IPCConn()
   : wxConnection()
   {
   };

   ~IPCConn()
   {
   };

   bool OnExec(const wxString & WXUNUSED(topic),
               const wxString & data)
   {
      // Add the filename to the queue.  It will be opened by
      // the OnTimer() event when it is safe to do so.
      ofqueue.push_back(data);

      return true;
   }
};

class IPCServ final : public wxServer
{
public:
   IPCServ(const wxString & appl)
   : wxServer()
   {
      Create(appl);
   };

   ~IPCServ()
   {
   };

   wxConnectionBase *OnAcceptConnection(const wxString & topic) override
   {
      if (topic != IPC_TOPIC) {
         return NULL;
      }

      // Trust wxWidgets framework to DELETE it
      return safenew IPCConn();
   };
};

#if defined(__WXMAC__)

IMPLEMENT_APP_NO_MAIN(AudacityApp)
IMPLEMENT_WX_THEME_SUPPORT

int main(int argc, char *argv[])
{
   wxDISABLE_DEBUG_SUPPORT();

   return wxEntry(argc, argv);
}

#elif defined(__WXGTK__) && defined(NDEBUG)

IMPLEMENT_APP_NO_MAIN(AudacityApp)
IMPLEMENT_WX_THEME_SUPPORT

int main(int argc, char *argv[])
{
   wxDISABLE_DEBUG_SUPPORT();

   // Bug #1986 workaround - This doesn't actually reduce the number of 
   // messages, it simply hides them in Release builds. We'll probably
   // never be able to get rid of the messages entirely, but we should
   // look into what's causing them, so allow them to show in Debug
   // builds.
   stdout = freopen("/dev/null", "w", stdout);
   stderr = freopen("/dev/null", "w", stderr);

   return wxEntry(argc, argv);
}

#else
IMPLEMENT_APP(AudacityApp)
#endif

#ifdef __WXMAC__

// in response of an open-document apple event
void AudacityApp::MacOpenFile(const wxString &fileName)
{
   ofqueue.push_back(fileName);
}

// in response of a print-document apple event
void AudacityApp::MacPrintFile(const wxString &fileName)
{
   ofqueue.push_back(fileName);
}

// in response of a open-application apple event
void AudacityApp::MacNewFile()
{
   if (!gInited)
      return;

   // This method should only be used on the Mac platform
   // when no project windows are open.

   if (AllProjects{}.empty())
      (void) ProjectManager::New();
}

#endif //__WXMAC__

// IPC communication
#define ID_IPC_SERVER   6200
#define ID_IPC_SOCKET   6201

// we don't really care about the timer id, but set this value just in case we do in the future
#define kAudacityAppTimerID 0

BEGIN_EVENT_TABLE(AudacityApp, wxApp)
   EVT_IDLE( AudacityApp::OnIdle )

   EVT_QUERY_END_SESSION(AudacityApp::OnQueryEndSession)
   EVT_END_SESSION(AudacityApp::OnEndSession)

   EVT_TIMER(kAudacityAppTimerID, AudacityApp::OnTimer)
#ifdef __WXMAC__
   EVT_MENU(wxID_NEW, AudacityApp::OnMenuNew)
   EVT_MENU(wxID_OPEN, AudacityApp::OnMenuOpen)
   EVT_MENU(wxID_ABOUT, AudacityApp::OnMenuAbout)
   EVT_MENU(wxID_PREFERENCES, AudacityApp::OnMenuPreferences)
#endif

   // Associate the handler with the menu id on all operating systems, even
   // if they don't have an application menu bar like in macOS, so that
   // other parts of the program can send the application a shut-down
   // event
   EVT_MENU(wxID_EXIT, AudacityApp::OnMenuExit)

#ifndef __WXMSW__
   EVT_SOCKET(ID_IPC_SERVER, AudacityApp::OnServerEvent)
   EVT_SOCKET(ID_IPC_SOCKET, AudacityApp::OnSocketEvent)
#endif

   // Recent file event handlers.
   EVT_MENU(FileHistory::ID_RECENT_CLEAR, AudacityApp::OnMRUClear)
   EVT_MENU_RANGE(FileHistory::ID_RECENT_FIRST, FileHistory::ID_RECENT_LAST,
      AudacityApp::OnMRUFile)

   // Handle AppCommandEvents (usually from a script)
   EVT_APP_COMMAND(wxID_ANY, AudacityApp::OnReceiveCommand)

   // Global ESC key handling
   EVT_KEY_DOWN(AudacityApp::OnKeyDown)
END_EVENT_TABLE()

// backend for OnMRUFile
// TODO: Would be nice to make this handle not opening a file with more panache.
//  - Inform the user if DefaultOpenPath not set.
//  - Switch focus to correct instance of project window, if already open.
bool AudacityApp::MRUOpen(const FilePath &fullPathStr) {
   // Most of the checks below are copied from ProjectManager::OpenFiles.
   // - some rationalisation might be possible.

   auto pProj = GetActiveProject().lock();
   auto proj = pProj.get();

   if (!fullPathStr.empty())
   {
      // verify that the file exists
      if (wxFile::Exists(fullPathStr))
      {
         FileNames::UpdateDefaultPath(FileNames::Operation::Open, ::wxPathOnly(fullPathStr));

         // Make sure it isn't already open.
         // Test here even though AudacityProject::OpenFile() also now checks, because
         // that method does not return the bad result.
         // That itself may be a FIXME.
         if (ProjectFileManager::IsAlreadyOpen(fullPathStr))
            return false;

         //! proj may be null
         ( void ) ProjectManager::OpenProject( proj, fullPathStr,
               true /* addtohistory */, false /* reuseNonemptyProject */ );
      }
      else {
         // File doesn't exist - remove file from history
         AudacityMessageBox(
            XO(
"%s could not be found.\n\nIt has been removed from the list of recent files.")
               .Format(fullPathStr) );
         return(false);
      }
   }
   return(true);
}

bool AudacityApp::SafeMRUOpen(const wxString &fullPathStr)
{
   return GuardedCall< bool >( [&]{ return MRUOpen( fullPathStr ); } );
}

void AudacityApp::OnMRUClear(wxCommandEvent& WXUNUSED(event))
{
   FileHistory::Global().Clear();
}

//vvv Basically, anything from Recent Files is treated as a .aup3, until proven otherwise,
// then it tries to Import(). Very questionable handling, imo.
// Better, for example, to check the file type early on.
void AudacityApp::OnMRUFile(wxCommandEvent& event) {
   int n = event.GetId() - FileHistory::ID_RECENT_FIRST;
   auto &history = FileHistory::Global();
   const auto &fullPathStr = history[ n ];

   // Try to open only if not already open.
   // Test IsAlreadyOpen() here even though AudacityProject::MRUOpen() also now checks,
   // because we don't want to Remove() just because it already exists,
   // and AudacityApp::OnMacOpenFile() calls MRUOpen() directly.
   // that method does not return the bad result.
   // PRL: Don't call SafeMRUOpen
   // -- if open fails for some exceptional reason of resource exhaustion that
   // the user can correct, leave the file in history.
   if (!ProjectFileManager::IsAlreadyOpen(fullPathStr) && !MRUOpen(fullPathStr))
      history.Remove(n);
}

void AudacityApp::OnTimer(wxTimerEvent& WXUNUSED(event))
{
   // Filenames are queued when Audacity receives a few of the
   // AppleEvent messages (via wxWidgets).  So, open any that are
   // in the queue and clean the queue.
   if (gInited) {
      if (ofqueue.size()) {
         // Load each file on the queue
         while (ofqueue.size()) {
            wxString name;
            name.swap(ofqueue[0]);
            ofqueue.erase( ofqueue.begin() );

            // Get the user's attention if no file name was specified
            if (name.empty()) {
               // Get the users attention
               if (auto project = GetActiveProject().lock()) {
                  auto &window = GetProjectFrame( *project );
                  window.Maximize();
                  window.Raise();
                  window.RequestUserAttention();
               }
               continue;
            }

            // TODO: Handle failures better.
            // Some failures are OK, e.g. file not found, just would-be-nices to do better,
            // so FAIL_MSG is more a case of an enhancement request than an actual  problem.
            // LL:  In all but one case an appropriate message is already displayed.  The
            //      instance that a message is NOT displayed is when a failure to write
            //      to the config file has occurred.
            // PRL: Catch any exceptions, don't try this file again, continue to
            // other files.
            if (!SafeMRUOpen(name)) {
               // Just log it.  Assertion failure is not appropriate on valid
               // defensive path against bad file data.
               wxLogMessage(wxT("MRUOpen failed"));
            }
         }
      }
   }
}

#if defined(__WXMSW__)
#define WL(lang, sublang) (lang), (sublang),
#else
#define WL(lang,sublang)
#endif

#if wxCHECK_VERSION(3, 0, 1)
wxLanguageInfo userLangs[] =
{
   // Bosnian is defined in wxWidgets already
//   { wxLANGUAGE_USER_DEFINED, wxT("bs"), WL(0, SUBLANG_DEFAULT) wxT("Bosnian"), wxLayout_LeftToRight },

   { wxLANGUAGE_USER_DEFINED, wxT("eu"), WL(0, SUBLANG_DEFAULT) wxT("Basque"), wxLayout_LeftToRight },
};
#endif

void AudacityApp::OnFatalException()
{
#if defined(HAS_CRASH_REPORT)
   CrashReport::Generate(wxDebugReport::Context_Exception);
#endif

   exit(-1);
}


#ifdef _MSC_VER
// If this is compiled with MSVC (Visual Studio)
#pragma warning( push )
#pragma warning( disable : 4702) // unreachable code warning.
#endif //_MSC_VER

bool AudacityApp::OnExceptionInMainLoop()
{
   // This function is invoked from catch blocks in the wxWidgets framework,
   // and throw; without argument re-throws the exception being handled,
   // letting us dispatch according to its type.

   try { throw; }
   catch ( AudacityException &e ) {
      (void)e;// Compiler food
      // Here is the catch-all for our own exceptions

      // Use CallAfter to delay this to the next pass of the event loop,
      // rather than risk doing it inside stack unwinding.
      auto pProject = ::GetActiveProject().lock();
      auto pException = std::current_exception();
      CallAfter( [pException, pProject] {

         // Restore the state of the project to what it was before the
         // failed operation
         if (pProject) {
            ProjectHistory::Get( *pProject ).RollbackState();

            // Forget pending changes in the TrackList
            TrackList::Get( *pProject ).ClearPendingTracks();

            ProjectWindow::Get( *pProject ).RedrawProject();
         }

         // Give the user an alert
         try { std::rethrow_exception( pException ); }
         catch( AudacityException &e )
            { e.DelayedHandlerAction(); }

      } );

      // Don't quit the program
      return true;
   }
   catch ( ... ) {
      // There was some other type of exception we don't know.
      // Let the inherited function do throw; again and whatever else it does.
      return wxApp::OnExceptionInMainLoop();
   }
   // Shouldn't ever reach this line
   return false;
}
#ifdef _MSC_VER
#pragma warning( pop )
#endif //_MSC_VER

AudacityApp::AudacityApp()
{
#if defined(USE_BREAKPAD)
    InitBreakpad();
// Do not capture crashes in debug builds
#elif !defined(_DEBUG)
#if defined(HAS_CRASH_REPORT)
#if defined(wxUSE_ON_FATAL_EXCEPTION) && wxUSE_ON_FATAL_EXCEPTION
   wxHandleFatalExceptions();
#endif
#endif
#endif
}

AudacityApp::~AudacityApp()
{
}

// Some of the many initialization steps
void AudacityApp::OnInit0()
{
   // Inject basic GUI services behind the facade
   {
      static wxWidgetsBasicUI uiServices;
      (void)BasicUI::Install(&uiServices);
   }

   // Fire up SQLite
   if ( !ProjectFileIO::InitializeSQL() )
      this->CallAfter([]{
         ::AudacityMessageBox(
            XO("SQLite library failed to initialize.  Audacity cannot continue.") );
         QuitAudacity( true );
      });


   // cause initialization of wxWidgets' global logger target
   (void) AudacityLogger::Get();

#if defined(__WXMAC__)
   // Disable window animation
   wxSystemOptions::SetOption(wxMAC_WINDOW_PLAIN_TRANSITION, 1);
#endif

   // Some GTK themes produce larger combo boxes that make them taller
   // than our single toolbar height restriction.  This will remove some
   // of the extra space themes add.
#if defined(__WXGTK3__) && defined(HAVE_GTK)
   GtkWidget *combo = gtk_combo_box_new();
   GtkCssProvider *provider = gtk_css_provider_new();
   gtk_css_provider_load_from_data(GTK_CSS_PROVIDER(provider),
                                   ".linked entry,\n"
                                   ".linked button,\n"
                                   ".linked combobox box.linked button,\n"
                                   ".horizontal.linked entry,\n"
                                   ".horizontal.linked button,\n"
                                   ".horizontal.linked combobox box.linked button,\n"
                                   "combobox {\n"
                                   "   padding-top: 0px;\n"
                                   "   padding-bottom: 0px;\n"
                                   "   padding-left: 4px;\n"
                                   "   padding-right: 4px;\n"
                                   "   margin: 0px;\n"
                                   "   font-size: 95%;\n"
                                   "}", -1, NULL);
   gtk_style_context_add_provider_for_screen(gtk_widget_get_screen(combo),
                                             GTK_STYLE_PROVIDER (provider),
                                             GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
   g_object_unref(provider);
   g_object_unref(combo);
#elif defined(__WXGTK__) && defined(HAVE_GTK)
   gtk_rc_parse_string("style \"audacity\" {\n"
                       " GtkButton::inner_border = { 0, 0, 0, 0 }\n"
                       " GtkEntry::inner_border = { 0, 0, 0, 0 }\n"
                       " xthickness = 4\n"
                       " ythickness = 0\n"
                       "}\n"
                       "widget_class \"*GtkCombo*\" style \"audacity\"");
#endif

   wxTheApp->SetAppName(AppName);
   // Explicitly set since OSX will use it for the "Quit" menu item
   wxTheApp->SetAppDisplayName(AppName);
   wxTheApp->SetVendorName(AppName);

   ::wxInitAllImageHandlers();

   // AddHandler takes ownership
   wxFileSystem::AddHandler(safenew wxZipFSHandler);
}
#endif

void FileNames::InitializePathList()
{
   auto &standardPaths = wxStandardPaths::Get();
   const auto programPath = standardPaths.GetExecutablePath();

   //
   // Paths: set search path and temp dir path
   //
   FilePaths audacityPathList;

#ifdef __WXGTK__
   const auto portablePrefix = wxPathOnly(wxPathOnly(programPath));

   // Make sure install prefix is set so wxStandardPath resolves paths properly
   if (wxDirExists(portablePrefix + L"/share/audacity")) {
      // use prefix relative to executable location to make Audacity portable
      standardPaths.SetInstallPrefix(portablePrefix);
   } else {
      // fallback to hard-coded prefix set during configuration
      standardPaths.SetInstallPrefix(wxT(INSTALL_PREFIX));
   }
   wxString installPrefix = standardPaths.GetInstallPrefix();

   /* Search path (for plug-ins, translations etc) is (in this order):
      * The AUDACITY_PATH environment variable
      * The current directory
      * The user's "~/.audacity-data" or "Portable Settings" directory
      * The user's "~/.audacity-files" directory
      * The "share" and "share/doc" directories in their install path */
   wxString home = wxGetHomeDir();

   wxString envTempDir = wxGetenv(wxT("TMPDIR"));
   if (!envTempDir.empty()) {
      /* On Unix systems, the environment variable TMPDIR may point to
         an unusual path when /tmp and /var/tmp are not desirable. */
      TempDirectory::SetDefaultTempDir( wxString::Format(
         wxT("%s/audacity-%s"), envTempDir, wxGetUserId() ) );
   } else {
      /* On Unix systems, the default temp dir is in /var/tmp. */
      TempDirectory::SetDefaultTempDir( wxString::Format(
         wxT("/var/tmp/audacity-%s"), wxGetUserId() ) );
   }

// DA: Path env variable.
#ifndef EXPERIMENTAL_DA
   wxString pathVar = wxGetenv(wxT("AUDACITY_PATH"));
#else
   wxString pathVar = wxGetenv(wxT("DARKAUDACITY_PATH"));
#endif
   if (!pathVar.empty())
      FileNames::AddMultiPathsToPathList(pathVar, audacityPathList);
   FileNames::AddUniquePathToPathList(::wxGetCwd(), audacityPathList);

   const auto progPath = wxPathOnly(programPath);

   FileNames::AddUniquePathToPathList(progPath, audacityPathList);
   // Add the path to modules:
   FileNames::AddUniquePathToPathList(progPath + L"/lib/audacity", audacityPathList);

#if !defined(__WXMSW__)
   // On Unix systems, the common directory structure is
   // .../bin
   // .../lib
   const wxString progParentPath = wxPathOnly(progPath);

   if (!progParentPath.IsEmpty())
   {
      FileNames::AddUniquePathToPathList(progParentPath + L"/lib/audacity", audacityPathList);
      FileNames::AddUniquePathToPathList(progParentPath + L"/lib", audacityPathList);
   }
#endif

   FileNames::AddUniquePathToPathList(FileNames::DataDir(), audacityPathList);

#ifdef AUDACITY_NAME
   FileNames::AddUniquePathToPathList(wxString::Format(wxT("%s/.%s-files"),
      home, wxT(AUDACITY_NAME)),
      audacityPathList);
   FileNames::AddUniquePathToPathList(FileNames::ModulesDir(),
      audacityPathList);
   FileNames::AddUniquePathToPathList(wxString::Format(installPrefix + L"/share/%s", wxT(AUDACITY_NAME)),
      audacityPathList);
   FileNames::AddUniquePathToPathList(wxString::Format(installPrefix + L"/share/doc/%s", wxT(AUDACITY_NAME)),
      audacityPathList);
#else //AUDACITY_NAME
   FileNames::AddUniquePathToPathList(wxString::Format(wxT("%s/.audacity-files"),
      home),
      audacityPathList);
   FileNames::AddUniquePathToPathList(FileNames::ModulesDir(),
      audacityPathList);
   FileNames::AddUniquePathToPathList(installPrefix + L"/share/audacity",
      audacityPathList);
   FileNames::AddUniquePathToPathList(installPrefix + L"/share/doc/audacity",
      audacityPathList);
#endif //AUDACITY_NAME

   FileNames::AddUniquePathToPathList(installPrefix + L"/share/locale",
      audacityPathList);

   FileNames::AddUniquePathToPathList(wxString::Format(wxT("./locale")),
      audacityPathList);

#endif //__WXGTK__

// JKC Bug 1220: Use path based on home directory on WXMAC
#ifdef __WXMAC__
   wxFileName tmpFile;
   tmpFile.AssignHomeDir();
   wxString tmpDirLoc = tmpFile.GetPath(wxPATH_GET_VOLUME);
#else
   wxFileName tmpFile;
   tmpFile.AssignTempFileName(wxT("nn"));
   wxString tmpDirLoc = tmpFile.GetPath(wxPATH_GET_VOLUME);
   ::wxRemoveFile(tmpFile.GetFullPath());
#endif



   // On Mac and Windows systems, use the directory which contains Audacity.
#ifdef __WXMSW__
   // On Windows, the path to the Audacity program is programPath
   const auto progPath = wxPathOnly(programPath);
   FileNames::AddUniquePathToPathList(progPath, audacityPathList);
   FileNames::AddUniquePathToPathList(progPath + wxT("\\Languages"), audacityPathList);

   // See bug #1271 for explanation of location
   tmpDirLoc = FileNames::MkDir(wxStandardPaths::Get().GetUserLocalDataDir());
   TempDirectory::SetDefaultTempDir( wxString::Format(
      wxT("%s\\SessionData"), tmpDirLoc ) );
#endif //__WXWSW__

#ifdef __WXMAC__
   // On Mac OS X, the path to the Audacity program is programPath
   const auto progPath = wxPathOnly(programPath);

   FileNames::AddUniquePathToPathList(progPath, audacityPathList);
   // If Audacity is a "bundle" package, then the root directory is
   // the great-great-grandparent of the directory containing the executable.
   //FileNames::AddUniquePathToPathList(progPath + wxT("/../../../"), audacityPathList);

   // These allow for searching the "bundle"
   FileNames::AddUniquePathToPathList(
      progPath + wxT("/../"), audacityPathList);
   FileNames::AddUniquePathToPathList(
      progPath + wxT("/../Resources"), audacityPathList);

   // JKC Bug 1220: Using an actual temp directory for session data on Mac was
   // wrong because it would get cleared out on a reboot.
   TempDirectory::SetDefaultTempDir( wxString::Format(
      wxT("%s/Library/Application Support/audacity/SessionData"), tmpDirLoc) );

   //TempDirectory::SetDefaultTempDir( wxString::Format(
   //   wxT("%s/audacity-%s"),
   //   tmpDirLoc,
   //   wxGetUserId() ) );
#endif //__WXMAC__

   FileNames::SetAudacityPathList( std::move( audacityPathList ) );
}
