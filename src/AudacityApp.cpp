/**********************************************************************

  Audacity: A Digital Audio Editor

  AudacityApp.cpp

  Dominic Mazzoni

******************************************************************//**

\class AudacityApp
\brief AudacityApp is the 'main' class for Audacity

It handles initialization and termination by subclassing wxApp.

*//*******************************************************************/

#if 0
// This may be used to debug memory leaks.
// See: Visual Leak Dectector @ http://vld.codeplex.com/
#include <vld.h>
#endif

#include "Audacity.h" // This should always be included first
#include "AudacityApp.h"
#include "TranslatableStringArray.h"

#include <wx/defs.h>
#include <wx/app.h>
#include <wx/bitmap.h>
#include <wx/docview.h>
#include <wx/event.h>
#include <wx/ipc.h>
#include <wx/log.h>
#include <wx/window.h>
#include <wx/intl.h>
#include <wx/menu.h>
#include <wx/msgdlg.h>
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
#endif

// chmod, lstat, geteuid
#ifdef __UNIX__
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
#endif

#include "AudacityLogger.h"
#include "AboutDialog.h"
#include "AColor.h"
#include "AudioIO.h"
#include "Benchmark.h"
#include "DirManager.h"
#include "commands/CommandHandler.h"
#include "commands/AppCommandEvent.h"
#include "effects/Contrast.h"
#include "widgets/ASlider.h"
#include "FFmpeg.h"
#include "Internat.h"
#include "LangChoice.h"
#include "Languages.h"
#include "PluginManager.h"
#include "Prefs.h"
#include "Project.h"
#include "Screenshot.h"
#include "Sequence.h"
#include "WaveTrack.h"
#include "Internat.h"
#include "prefs/PrefsDialog.h"
#include "Theme.h"
#include "PlatformCompatibility.h"
#include "FileNames.h"
#include "AutoRecovery.h"
#include "SplashDialog.h"
#include "FFT.h"
#include "BlockFile.h"
#include "ondemand/ODManager.h"
#include "commands/Keyboard.h"
#include "widgets/ErrorDialog.h"
#include "prefs/DirectoriesPrefs.h"
#include "tracks/ui/Scrubbing.h"

//temporarilly commented out till it is added to all projects
//#include "Profiler.h"

#include "ModuleManager.h"

#include "import/Import.h"

#include "Experimental.h"

#if defined(EXPERIMENTAL_CRASH_REPORT)
#include <wx/debugrpt.h>
#include <wx/evtloop.h>
#include <wx/textdlg.h>
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

// Windows specific linker control...only needed once so
// this is a good place (unless we want to add another file).
#if defined(__WXMSW__)
//#if wxCHECK_VERSION(3, 0, 2) && !wxCHECK_VERSION(3, 1, 0)
#include <wx/init.h>
//#endif
// These lines ensure that Audacity gets WindowsXP themes.
// Without them we get the old-style Windows98/2000 look under XP.
#  if !defined(__WXWINCE__)
#     pragma comment(linker,"\"/manifestdependency:type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='*' publicKeyToken='6595b64144ccf1df' language='*'\"")
#  endif

// These lines allows conditional inclusion of the various libraries
// that Audacity can use.

#  if defined(USE_LIBFLAC)
#     pragma comment(lib, "libflac++")
#     pragma comment(lib, "libflac")
#  endif

#  if defined(USE_LIBID3TAG)
#     pragma comment(lib, "libid3tag")
#  endif

#  if defined(USE_LIBMAD)
#     pragma comment(lib, "libmad")
#  endif

#  if defined(USE_LIBTWOLAME)
#     pragma comment(lib, "twolame")
#  endif

#  if defined(USE_LIBVORBIS)
#     pragma comment(lib, "libogg")
#     pragma comment(lib, "libvorbis")
#  endif

#  if defined(USE_LV2)
#     pragma comment(lib, "lv2")
#  endif

#  if defined(USE_MIDI)
#     pragma comment(lib, "portsmf")
#     endif

#  if defined(EXPERIMENTAL_MIDI_OUT)
#     pragma comment(lib, "portmidi")
#  endif

#  if defined(EXPERIMENTAL_SCOREALIGN)
#     pragma comment(lib, "libscorealign")
#  endif

#  if defined(USE_NYQUIST)
#     pragma comment(lib, "libnyquist")
#  endif

#  if defined(USE_PORTMIXER)
#     pragma comment(lib, "portmixer")
#  endif

#  if defined(USE_SBSMS)
#     pragma comment(lib, "sbsms")
#  endif

#  if defined(USE_SOUNDTOUCH)
#     pragma comment(lib, "soundtouch")
#  endif

#  if defined(USE_VAMP)
#     pragma comment(lib, "libvamp")
#  endif

#  if defined(__WXDEBUG__)
#     define D "d"
#  else
#     define D ""
#  endif
#  if wxCHECK_VERSION(3, 1, 0)
#     define V "31"
#  elif wxCHECK_VERSION(3, 0, 0)
#     define V "30"
#  else
#     define V "28"
#  endif

#  if defined(EXPERIMENTAL_CRASH_REPORT)
#     pragma comment(lib, "wxmsw" V "u" D "_qa")
#  endif
#  pragma comment(lib, "wxbase" V "u" D)
#  pragma comment(lib, "wxbase" V "u" D "_net")
#  pragma comment(lib, "wxmsw"  V "u" D "_adv")
#  pragma comment(lib, "wxmsw"  V "u" D "_core")
#  pragma comment(lib, "wxmsw"  V "u" D "_html")
#  pragma comment(lib, "wxpng"        D)
#  pragma comment(lib, "wxzlib"       D)
#  pragma comment(lib, "wxjpeg"       D)
#  pragma comment(lib, "wxtiff"       D)

#  undef V
#  undef D

#endif //(__WXMSW__)

#include "../images/AudacityLogoWithName.xpm"

////////////////////////////////////////////////////////////
/// Custom events
////////////////////////////////////////////////////////////

DEFINE_EVENT_TYPE(EVT_OPEN_AUDIO_FILE);
DEFINE_EVENT_TYPE(EVT_LANGUAGE_CHANGE);

#if 0
#ifdef __WXGTK__
static void wxOnAssert(const wxChar *fileName, int lineNumber, const wxChar *msg)
{
   if (msg)
      printf("ASSERTION FAILED: %s\n%s: %d\n", (const char *)wxString(msg).mb_str(), (const char *)wxString(fileName).mb_str(), lineNumber);
   else
      printf("ASSERTION FAILED!\n%s: %d\n", (const char *)wxString(fileName).mb_str(), lineNumber);

   // Force core dump
   int *i = 0;
   if (*i)
      exit(1);

   exit(0);
}
#endif
#endif

static bool gInited = false;
bool gIsQuitting = false;

void QuitAudacity(bool bForce)
{
   if (gIsQuitting)
      return;

   gIsQuitting = true;

   wxTheApp->SetExitOnFrameDelete(true);

   // Try to close each open window.  If the user hits Cancel
   // in a Save Changes dialog, don't continue.
   // BG: unless force is true

   // BG: Are there any projects open?
   //-   if (!gAudacityProjects.IsEmpty())
/*start+*/
   if (gAudacityProjects.empty())
   {
#ifdef __WXMAC__
      AudacityProject::DeleteClipboard();
#endif
   }
   else
/*end+*/
   {
      SaveWindowSize();
      while (gAudacityProjects.size())
      {
         // Closing the project has global side-effect
         // of deletion from gAudacityProjects
         if (bForce)
         {
            gAudacityProjects[0]->Close(true);
         }
         else
         {
            if (!gAudacityProjects[0]->Close())
            {
               gIsQuitting = false;
               return;
            }
         }
      }
   }

   ModuleManager::Get().Dispatch(AppQuiting);

#ifdef EXPERIMENTAL_SCOREALIGN
   CloseScoreAlignDialog();
#endif
   CloseScreenshotTools();

   //release ODManager Threads
   ODManager::Quit();

   //print out profile if we have one by deleting it
   //temporarilly commented out till it is added to all projects
   //DELETE Profiler::Instance();

   //remove our logger
   std::unique_ptr<wxLog>{ wxLog::SetActiveTarget(NULL) }; // DELETE

   if (bForce)
   {
      wxExit();
   }
}

void QuitAudacity()
{
   QuitAudacity(false);
}

void SaveWindowSize()
{
   if (wxGetApp().GetWindowRectAlreadySaved())
   {
      return;
   }
   bool validWindowForSaveWindowSize = FALSE;
   AudacityProject * validProject = NULL;
   bool foundIconizedProject = FALSE;
   size_t numProjects = gAudacityProjects.size();
   for (size_t i = 0; i < numProjects; i++)
   {
      if (!gAudacityProjects[i]->IsIconized()) {
         validWindowForSaveWindowSize = TRUE;
         validProject = gAudacityProjects[i].get();
         i = numProjects;
      }
      else
         foundIconizedProject =  TRUE;

   }
   if (validWindowForSaveWindowSize)
   {
      wxRect windowRect = validProject->GetRect();
      wxRect normalRect = validProject->GetNormalizedWindowState();
      bool wndMaximized = validProject->IsMaximized();
      gPrefs->Write(wxT("/Window/X"), windowRect.GetX());
      gPrefs->Write(wxT("/Window/Y"), windowRect.GetY());
      gPrefs->Write(wxT("/Window/Width"), windowRect.GetWidth());
      gPrefs->Write(wxT("/Window/Height"), windowRect.GetHeight());
      gPrefs->Write(wxT("/Window/Maximized"), wndMaximized);
      gPrefs->Write(wxT("/Window/Normal_X"), normalRect.GetX());
      gPrefs->Write(wxT("/Window/Normal_Y"), normalRect.GetY());
      gPrefs->Write(wxT("/Window/Normal_Width"), normalRect.GetWidth());
      gPrefs->Write(wxT("/Window/Normal_Height"), normalRect.GetHeight());
      gPrefs->Write(wxT("/Window/Iconized"), FALSE);
   }
   else
   {
      if (foundIconizedProject) {
         validProject = gAudacityProjects[0].get();
         bool wndMaximized = validProject->IsMaximized();
         wxRect normalRect = validProject->GetNormalizedWindowState();
         // store only the normal rectangle because the itemized rectangle
         // makes no sense for an opening project window
         gPrefs->Write(wxT("/Window/X"), normalRect.GetX());
         gPrefs->Write(wxT("/Window/Y"), normalRect.GetY());
         gPrefs->Write(wxT("/Window/Width"), normalRect.GetWidth());
         gPrefs->Write(wxT("/Window/Height"), normalRect.GetHeight());
         gPrefs->Write(wxT("/Window/Maximized"), wndMaximized);
         gPrefs->Write(wxT("/Window/Normal_X"), normalRect.GetX());
         gPrefs->Write(wxT("/Window/Normal_Y"), normalRect.GetY());
         gPrefs->Write(wxT("/Window/Normal_Width"), normalRect.GetWidth());
         gPrefs->Write(wxT("/Window/Normal_Height"), normalRect.GetHeight());
         gPrefs->Write(wxT("/Window/Iconized"), TRUE);
      }
      else {
         // this would be a very strange case that might possibly occur on the Mac
         // Audacity would have to be running with no projects open
         // in this case we are going to write only the default values
         wxRect defWndRect;
         GetDefaultWindowRect(&defWndRect);
         gPrefs->Write(wxT("/Window/X"), defWndRect.GetX());
         gPrefs->Write(wxT("/Window/Y"), defWndRect.GetY());
         gPrefs->Write(wxT("/Window/Width"), defWndRect.GetWidth());
         gPrefs->Write(wxT("/Window/Height"), defWndRect.GetHeight());
         gPrefs->Write(wxT("/Window/Maximized"), FALSE);
         gPrefs->Write(wxT("/Window/Normal_X"), defWndRect.GetX());
         gPrefs->Write(wxT("/Window/Normal_Y"), defWndRect.GetY());
         gPrefs->Write(wxT("/Window/Normal_Width"), defWndRect.GetWidth());
         gPrefs->Write(wxT("/Window/Normal_Height"), defWndRect.GetHeight());
         gPrefs->Write(wxT("/Window/Iconized"), FALSE);
      }
   }
   gPrefs->Flush();
   wxGetApp().SetWindowRectAlreadySaved(TRUE);
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

   if (gAudacityProjects.empty()) {
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
      mArgv[0] = strdup("Audacity");

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

      gnome_program_init(mArgv[0],
                         "1.0",
                         libgnomeui_module_info_get(),
                         1,
                         mArgv,
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

      free(mArgv[0]);
   }

 private:

   char *mArgv[1];
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
      ofqueue.Add(data);
     
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
// This should be removed when Lame and FFmpeg support is converted
// from loadable libraries to commands.
//
// The purpose of this is to give the user more control over where libraries
// such as Lame and FFmpeg get loaded from.
//
// Since absolute pathnames are used when loading these libraries, the normal search
// path would be DYLD_LIBRARY_PATH, absolute path, DYLD_FALLBACK_LIBRARY_PATH.  This
// means that DYLD_LIBRARY_PATH can override what the user actually wants.
//
// So, we simply clear DYLD_LIBRARY_PATH to allow the users choice to be the first
// one tried.
IMPLEMENT_APP_NO_MAIN(AudacityApp)
IMPLEMENT_WX_THEME_SUPPORT

int main(int argc, char *argv[])
{
   if (getenv("DYLD_LIBRARY_PATH")) {
      extern char **environ;

      unsetenv("DYLD_LIBRARY_PATH");
      execve(argv[0], argv, environ);
   }

   wxDISABLE_DEBUG_SUPPORT();

   return wxEntry(argc, argv);
}

#elif defined(__WXMSW__) && !wxCHECK_VERSION(3, 1, 0)
// Disable telling Windows that we support HiDPI displays.  It is forced on
// in wxWidget versions between 3.0.0 and 3.1.0.
IMPLEMENT_APP_NO_MAIN(AudacityApp)
IMPLEMENT_WX_THEME_SUPPORT

extern "C" int WINAPI WinMain(HINSTANCE hInstance,
                              HINSTANCE hPrevInstance,
                              wxCmdLineArgType WXUNUSED(lpCmdLine),
                              int nCmdShow)
{
   wxDISABLE_DEBUG_SUPPORT();

   // Disable setting of HiDPI aware mode
   wxMSWDisableSettingHighDPIAware();

   /* NB: We pass NULL in place of lpCmdLine to behave the same as  */
   /*     Borland-specific wWinMain() above. If it becomes needed   */
   /*     to pass lpCmdLine to wxEntry() here, you'll have to fix   */
   /*     wWinMain() above too.                                     */
   return wxEntry(hInstance, hPrevInstance, NULL, nCmdShow);
}

#else
IMPLEMENT_APP(AudacityApp)
#endif

#ifdef __WXMAC__

// in response of an open-document apple event
void AudacityApp::MacOpenFile(const wxString &fileName)
{
   ofqueue.Add(fileName);
}

// in response of a print-document apple event
void AudacityApp::MacPrintFile(const wxString &fileName)
{
   ofqueue.Add(fileName);
}

// in response of a open-application apple event
void AudacityApp::MacNewFile()
{
   if (!gInited)
      return;

   // This method should only be used on the Mac platform
   // when no project windows are open.

   if (gAudacityProjects.size() == 0) {
      CreateNewAudacityProject();
   }
}

#endif //__WXMAC__

#define ID_RECENT_CLEAR 6100
#define ID_RECENT_FIRST 6101
#define ID_RECENT_LAST  6112

// IPC communication
#define ID_IPC_SERVER   6200
#define ID_IPC_SOCKET   6201

// we don't really care about the timer id, but set this value just in case we do in the future
#define kAudacityAppTimerID 0

BEGIN_EVENT_TABLE(AudacityApp, wxApp)
   EVT_QUERY_END_SESSION(AudacityApp::OnEndSession)

   EVT_TIMER(kAudacityAppTimerID, AudacityApp::OnTimer)
#ifdef __WXMAC__
   EVT_MENU(wxID_NEW, AudacityApp::OnMenuNew)
   EVT_MENU(wxID_OPEN, AudacityApp::OnMenuOpen)
   EVT_MENU(wxID_ABOUT, AudacityApp::OnMenuAbout)
   EVT_MENU(wxID_PREFERENCES, AudacityApp::OnMenuPreferences)
   EVT_MENU(wxID_EXIT, AudacityApp::OnMenuExit)
#endif

#ifndef __WXMSW__
   EVT_SOCKET(ID_IPC_SERVER, AudacityApp::OnServerEvent)
   EVT_SOCKET(ID_IPC_SOCKET, AudacityApp::OnSocketEvent)
#endif

   // Recent file event handlers.
   EVT_MENU(ID_RECENT_CLEAR, AudacityApp::OnMRUClear)
   EVT_MENU_RANGE(ID_RECENT_FIRST, ID_RECENT_LAST, AudacityApp::OnMRUFile)

   // Handle AppCommandEvents (usually from a script)
   EVT_APP_COMMAND(wxID_ANY, AudacityApp::OnReceiveCommand)

   // Global ESC key handling
   EVT_KEY_DOWN(AudacityApp::OnKeyDown)
END_EVENT_TABLE()

// backend for OnMRUFile
// TODO: Would be nice to make this handle not opening a file with more panache.
//  - Inform the user if DefaultOpenPath not set.
//  - Switch focus to correct instance of project window, if already open.
bool AudacityApp::MRUOpen(const wxString &fullPathStr) {
   // Most of the checks below are copied from AudacityProject::OpenFiles.
   // - some rationalisation might be possible.

   AudacityProject *proj = GetActiveProject();

   if (!fullPathStr.IsEmpty())
   {
      // verify that the file exists
      if (wxFile::Exists(fullPathStr))
      {
         if (!gPrefs->Write(wxT("/DefaultOpenPath"), wxPathOnly(fullPathStr)) ||
               !gPrefs->Flush())
            return false;

         // Make sure it isn't already open.
         // Test here even though AudacityProject::OpenFile() also now checks, because
         // that method does not return the bad result.
         // That itself may be a FIXME.
         if (AudacityProject::IsAlreadyOpen(fullPathStr))
            return false;

         // DMM: If the project is dirty, that means it's been touched at
         // all, and it's not safe to open a NEW project directly in its
         // place.  Only if the project is brand-NEW clean and the user
         // hasn't done any action at all is it safe for Open to take place
         // inside the current project.
         //
         // If you try to Open a NEW project inside the current window when
         // there are no tracks, but there's an Undo history, etc, then
         // bad things can happen, including data files moving to the NEW
         // project directory, etc.
         if (!proj || proj->GetDirty() || !proj->GetIsEmpty()) {
            proj = CreateNewAudacityProject();
         }
         // This project is clean; it's never been touched.  Therefore
         // all relevant member variables are in their initial state,
         // and it's okay to open a NEW project inside this window.
         proj->OpenFile(fullPathStr);
      }
      else {
         // File doesn't exist - remove file from history
         wxMessageBox(wxString::Format(_("%s could not be found.\n\nIt has been removed from the list of recent files."),
                      fullPathStr.c_str()));
         return(false);
      }
   }
   return(true);
}

void AudacityApp::OnMRUClear(wxCommandEvent& WXUNUSED(event))
{
   mRecentFiles->Clear();
}

//vvv Basically, anything from Recent Files is treated as a .aup, until proven otherwise,
// then it tries to Import(). Very questionable handling, imo.
// Better, for example, to check the file type early on.
void AudacityApp::OnMRUFile(wxCommandEvent& event) {
   int n = event.GetId() - ID_RECENT_FIRST;
   const wxString &fullPathStr = mRecentFiles->GetHistoryFile(n);

   // Try to open only if not already open.
   // Test IsAlreadyOpen() here even though AudacityProject::MRUOpen() also now checks,
   // because we don't want to RemoveFileFromHistory() just because it already exists,
   // and AudacityApp::OnMacOpenFile() calls MRUOpen() directly.
   // that method does not return the bad result.
   if (!AudacityProject::IsAlreadyOpen(fullPathStr) && !MRUOpen(fullPathStr))
      mRecentFiles->RemoveFileFromHistory(n);
}

void AudacityApp::OnTimer(wxTimerEvent& WXUNUSED(event))
{
   // Filenames are queued when Audacity receives the a few of the
   // AppleEvent messages (via wxWidgets).  So, open any that are
   // in the queue and clean the queue.
   if (gInited) {
      if (ofqueue.GetCount()) {
         // Load each file on the queue
         while (ofqueue.GetCount()) {
            wxString name;
            name.swap(ofqueue[0]);
            ofqueue.RemoveAt(0);

            // Get the user's attention if no file name was specified
            if (name.IsEmpty()) {
               // Get the users attention
               AudacityProject *project = GetActiveProject();
               if (project) {
                  project->Maximize();
                  project->Raise();
                  project->RequestUserAttention();
               }
               continue;
            }

            // TODO: Handle failures better.
            // Some failures are OK, e.g. file not found, just would-be-nices to do better,
            // so FAIL_MSG is more a case of an enhancement request than an actual  problem.
            // LL:  In all but one case an appropriate message is already displayed.  The
            //      instance that a message is NOT displayed is when a failure to write
            //      to the config file has occurred.
            if (!MRUOpen(name)) {
               wxFAIL_MSG(wxT("MRUOpen failed"));
            }
         }
      }
   }

   // Check if a warning for missing aliased files should be displayed
   if (ShouldShowMissingAliasedFileWarning()) {
      // find which project owns the blockfile
      // note: there may be more than 1, but just go with the first one.
      //size_t numProjects = gAudacityProjects.size();
      AudacityProject *offendingProject {};
      wxString missingFileName;

      {
         ODLocker locker { &m_LastMissingBlockFileLock };
         offendingProject =
            AProjectHolder{ m_LastMissingBlockFileProject }.get();
         missingFileName = m_LastMissingBlockFilePath;
      }

      // if there are no projects open, don't show the warning (user has closed it)
      if (offendingProject) {
         offendingProject->Iconize(false);
         offendingProject->Raise();

         wxString errorMessage = wxString::Format(_(
"One or more external audio files could not be found.\n\
It is possible they were moved, deleted, or the drive they \
were on was unmounted.\n\
Silence is being substituted for the affected audio.\n\
The first detected missing file is:\n\
%s\n\
There may be additional missing files.\n\
Choose File > Check Dependencies to view a list of \
locations of the missing files."), missingFileName.c_str());

         // if an old dialog exists, raise it if it is
         if (offendingProject->GetMissingAliasFileDialog()) {
            offendingProject->GetMissingAliasFileDialog()->Raise();
         } else {
            ShowAliasMissingDialog(offendingProject, _("Files Missing"),
                                   errorMessage, wxT(""), true);
         }
      }
      // Only show this warning once per event (playback/menu item/etc).
      SetMissingAliasedFileWarningShouldShow(false);
   }
}

void AudacityApp::MarkAliasedFilesMissingWarning(const AliasBlockFile *b)
{
   ODLocker locker { &m_LastMissingBlockFileLock };
   if (b) {
   size_t numProjects = gAudacityProjects.size();
      for (size_t ii = 0; ii < numProjects; ++ii) {
         // search each project for the blockfile
         if (gAudacityProjects[ii]->GetDirManager()->ContainsBlockFile(b)) {
            m_LastMissingBlockFileProject = gAudacityProjects[ii];
            break;
         }
      }
   }
   else
      m_LastMissingBlockFileProject = {};

   if (b)
      m_LastMissingBlockFilePath = b->GetAliasedFileName().GetFullPath();
   else
      m_LastMissingBlockFilePath = wxString{};
}

void AudacityApp::SetMissingAliasedFileWarningShouldShow(bool b)
{
   // Note that this is can be called by both the main thread and other threads.
   // I don't believe we need a mutex because we are checking zero vs non-zero,
   // and the setting from other threads will always be non-zero (true), and the
   // setting from the main thread is always false.
   m_aliasMissingWarningShouldShow = b;
   // reset the warnings as they were probably marked by a previous run
   if (m_aliasMissingWarningShouldShow) {
      MarkAliasedFilesMissingWarning( nullptr );
   }
}

bool AudacityApp::ShouldShowMissingAliasedFileWarning()
{
   ODLocker locker { &m_LastMissingBlockFileLock };
   auto ptr = m_LastMissingBlockFileProject.lock();
   return ptr && m_aliasMissingWarningShouldShow;
}

AudacityLogger *AudacityApp::GetLogger()
{
   // Use dynamic_cast so that we get a NULL ptr if we haven't yet
   // setup our logger.
   return dynamic_cast<AudacityLogger *>(wxLog::GetActiveTarget());
}

#if defined(__WXMSW__)
#define WL(lang, sublang) (lang), (sublang),
#else
#define WL(lang,sublang)
#endif

#if !wxCHECK_VERSION(3, 0, 1)
wxLanguageInfo userLangs[] =
{
   { wxLANGUAGE_USER_DEFINED, wxT("bs"), WL(0, SUBLANG_DEFAULT) wxT("Bosnian"), wxLayout_LeftToRight }
};
#endif

wxString AudacityApp::InitLang( const wxString & lang )
{
   wxString result = lang;

   mLocale.reset();

#if defined(__WXMAC__)
   // This should be reviewed again during the wx3 conversion.

   // On OSX, if the LANG environment variable isn't set when
   // using a language like Japanese, an assertion will trigger
   // because conversion to Japanese from "?" doesn't return a
   // valid length, so make OSX happy by defining/overriding
   // the LANG environment variable with U.S. English for now.
   wxSetEnv(wxT("LANG"), wxT("en_US.UTF-8"));
#endif

   const wxLanguageInfo *info = NULL;
   if (!lang.empty()) {
      info = wxLocale::FindLanguageInfo(lang);
      if (!info)
         ::wxMessageBox(wxString::Format(_("Language \"%s\" is unknown"), lang));
   }
   if (!info)
   {
      result = GetSystemLanguageCode();
      info = wxLocale::FindLanguageInfo(result);
      if (!info)
         return result;
   }
   mLocale = std::make_unique<wxLocale>(info->Language);

   for(unsigned int i=0; i<audacityPathList.GetCount(); i++)
      mLocale->AddCatalogLookupPathPrefix(audacityPathList[i]);

   // LL:  Must add the wxWidgets catalog manually since the search
   //      paths were not set up when mLocale was created.  The
   //      catalogs are search in LIFO order, so add wxstd first.
   mLocale->AddCatalog(wxT("wxstd"));

// AUDACITY_NAME is legitimately used on some *nix configurations.
#ifdef AUDACITY_NAME
   mLocale->AddCatalog(wxT(AUDACITY_NAME));
#else
   mLocale->AddCatalog(IPC_APPL);
#endif

   // Initialize internationalisation (number formats etc.)
   //
   // This must go _after_ creating the wxLocale instance because
   // creating the wxLocale instance sets the application-wide locale.

   Internat::Init();

   // Notify listeners of language changes
   {
      wxCommandEvent evt(EVT_LANGUAGE_CHANGE);
      ProcessEvent(evt);
   }

   return result;
}

void AudacityApp::OnFatalException()
{
#if defined(EXPERIMENTAL_CRASH_REPORT)
   GenerateCrashReport(wxDebugReport::Context_Exception);
#endif

   exit(-1);
}

#if defined(EXPERIMENTAL_CRASH_REPORT)
void AudacityApp::GenerateCrashReport(wxDebugReport::Context ctx)
{
   wxDebugReportCompress rpt;
   rpt.AddAll(ctx);

   wxFileName fn(FileNames::DataDir(), wxT("audacity.cfg"));
   rpt.AddFile(fn.GetFullPath(), _TS("Audacity Configuration"));
   rpt.AddFile(FileNames::PluginRegistry(), wxT("Plugin Registry"));
   rpt.AddFile(FileNames::PluginSettings(), wxT("Plugin Settings"));

   if (ctx == wxDebugReport::Context_Current)
   {
      rpt.AddText(wxT("audiodev.txt"), gAudioIO->GetDeviceInfo(), wxT("Audio Device Info"));
   }

   AudacityLogger *logger = GetLogger();
   if (logger)
   {
      rpt.AddText(wxT("log.txt"), logger->GetLog(), _TS("Audacity Log"));
   }

   bool ok = wxDebugReportPreviewStd().Show(rpt);

#if defined(__WXMSW__)
   wxEventLoop::SetCriticalWindow(NULL);
#endif

   if (ok && rpt.Process())
   {
      wxTextEntryDialog dlg(NULL,
                              _("Report generated to:"),
                              _("Audacity Support Data"),
                              rpt.GetCompressedFileName(),
                              wxOK | wxCENTER);
      dlg.SetName(dlg.GetTitle());
      dlg.ShowModal();

      wxLogMessage(wxT("Report generated to: %s"),
                     rpt.GetCompressedFileName().c_str());

      rpt.Reset();
   }
}
#endif

int AudacityApp::FilterEvent(wxEvent & event)
{
   (void)event;// compiler food (stops unused parameter warning)
#if !wxCHECK_VERSION(3, 0, 0) && defined(__WXGTK__)
   // On wxGTK, there's a focus issue where dialogs do not automatically pass focus
   // to the first child.  This means that you can use the keyboard to navigate within
   // the dialog.  Watching for the ACTIVATE event allows us to set the focus ourselves
   // when each dialog opens.
   //
   // See bug #57
   //
   if (event.GetEventType() == wxEVT_ACTIVATE)
   {
      wxActivateEvent & e = (wxActivateEvent &) event;

      if (e.GetEventObject() && e.GetActive() && e.GetEventObject()->IsKindOf(CLASSINFO(wxDialog)))
      {
         ((wxWindow *)e.GetEventObject())->SetFocus();
      }
   }
#endif

#ifdef __WXMAC__
   if (event.GetEventType() == wxEVT_ACTIVATE)
   {
      wxActivateEvent & e = static_cast<wxActivateEvent &>(event);

      const auto object = e.GetEventObject();
      if (object && e.GetActive() &&
          object->IsKindOf(CLASSINFO(wxWindow)))
      {
         const auto window = ((wxWindow *)e.GetEventObject());
         window->SetFocus();
         window->NavigateIn();
      }
   }
#endif

   return Event_Skip;
}

AudacityApp::AudacityApp()
{
// Do not capture crashes in debug builds
#if !defined(__WXDEBUG__)
#if defined(EXPERIMENTAL_CRASH_REPORT)
#if defined(wxUSE_ON_FATAL_EXCEPTION) && wxUSE_ON_FATAL_EXCEPTION
   wxHandleFatalExceptions();
#endif
#endif
#endif
}

AudacityApp::~AudacityApp()
{
}

// The `main program' equivalent, creating the windows and returning the
// main frame
bool AudacityApp::OnInit()
{
   // JKC: ANSWER-ME: Who actually added the event loop guarantor?
   // Although 'blame' says Leland, I think it came from a donated patch.

   // Ensure we have an event loop during initialization
   wxEventLoopGuarantor eventLoop;

   // wxWidgets will clean up the logger for the main thread, so we can say
   // safenew.  See:
   // http://docs.wxwidgets.org/3.0/classwx_log.html#a2525bf54fa3f31dc50e6e3cd8651e71d
   std::unique_ptr < wxLog >
      { wxLog::SetActiveTarget(safenew AudacityLogger) }; // DELETE old

   mLocale = NULL;

   m_aliasMissingWarningShouldShow = true;

#if defined(__WXMAC__)
   // Disable window animation
   wxSystemOptions::SetOption(wxMAC_WINDOW_PLAIN_TRANSITION, 1);
#endif

#ifdef AUDACITY_NAME
   wxString appName = wxT(AUDACITY_NAME);
#else
   wxString appName = wxT("Audacity");
#endif

   wxTheApp->SetAppName(appName);
   // Explicitly set since OSX will use it for the "Quit" menu item
   wxTheApp->SetAppDisplayName(appName);
   wxTheApp->SetVendorName(appName);

   // Unused strings that we want to be translated, even though
   // we're not using them yet...
   wxString future1 = _("Master Gain Control");

   ::wxInitAllImageHandlers();

   // AddHandler takes ownership
   wxFileSystem::AddHandler(safenew wxZipFSHandler);

   //
   // Paths: set search path and temp dir path
   //

#ifdef __WXGTK__
   /* Search path (for plug-ins, translations etc) is (in this order):
      * The AUDACITY_PATH environment variable
      * The current directory
      * The user's .audacity-files directory in their home directory
      * The "share" and "share/doc" directories in their install path */
   wxString home = wxGetHomeDir();

   /* On Unix systems, the default temp dir is in /var/tmp. */
   defaultTempDir.Printf(wxT("/var/tmp/audacity-%s"), wxGetUserId().c_str());

   wxString pathVar = wxGetenv(wxT("AUDACITY_PATH"));
   if (pathVar != wxT(""))
      AddMultiPathsToPathList(pathVar, audacityPathList);
   AddUniquePathToPathList(::wxGetCwd(), audacityPathList);

#ifdef AUDACITY_NAME
   AddUniquePathToPathList(wxString::Format(wxT("%s/.%s-files"),
      home.c_str(), wxT(AUDACITY_NAME)),
      audacityPathList);
   AddUniquePathToPathList(wxString::Format(wxT("%s/share/%s"),
      wxT(INSTALL_PREFIX), wxT(AUDACITY_NAME)),
      audacityPathList);
   AddUniquePathToPathList(wxString::Format(wxT("%s/share/doc/%s"),
      wxT(INSTALL_PREFIX), wxT(AUDACITY_NAME)),
      audacityPathList);
#else //AUDACITY_NAME
   AddUniquePathToPathList(wxString::Format(wxT("%s/.audacity-files"),
      home.c_str()),
      audacityPathList);
   AddUniquePathToPathList(wxString::Format(wxT("%s/share/audacity"),
      wxT(INSTALL_PREFIX)),
      audacityPathList);
   AddUniquePathToPathList(wxString::Format(wxT("%s/share/doc/audacity"),
      wxT(INSTALL_PREFIX)),
      audacityPathList);
#endif //AUDACITY_NAME

   AddUniquePathToPathList(wxString::Format(wxT("%s/share/locale"),
      wxT(INSTALL_PREFIX)),
      audacityPathList);

   AddUniquePathToPathList(wxString::Format(wxT("./locale")),
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
   // On Windows, the path to the Audacity program is in argv[0]
   wxString progPath = wxPathOnly(argv[0]);
   AddUniquePathToPathList(progPath, audacityPathList);
   AddUniquePathToPathList(progPath + wxT("\\Languages"), audacityPathList);

   // See bug #1271 for explanation of location
   tmpDirLoc = FileNames::MkDir(wxStandardPaths::Get().GetUserLocalDataDir());
   defaultTempDir.Printf(wxT("%s\\SessionData"),
      tmpDirLoc.c_str());
#endif //__WXWSW__

#ifdef __WXMAC__
   // On Mac OS X, the path to the Audacity program is in argv[0]
   wxString progPath = wxPathOnly(argv[0]);

   AddUniquePathToPathList(progPath, audacityPathList);
   // If Audacity is a "bundle" package, then the root directory is
   // the great-great-grandparent of the directory containing the executable.
   AddUniquePathToPathList(progPath + wxT("/../../../"), audacityPathList);

   // These allow for searching the "bundle"
   AddUniquePathToPathList(progPath + wxT("/../"), audacityPathList);
   AddUniquePathToPathList(progPath + wxT("/../Resources"), audacityPathList);

   // JKC Bug 1220: Using an actual temp directory for session data on Mac was
   // wrong because it would get cleared out on a reboot.
   defaultTempDir.Printf(wxT("%s/Library/Application\ Support/audacity/SessionData"),
      tmpDirLoc.c_str());

   //defaultTempDir.Printf(wxT("%s/audacity-%s"),
   //   tmpDirLoc.c_str(),
   //   wxGetUserId().c_str());
#endif //__WXMAC__

   // Define languanges for which we have translations, but that are not yet
   // supported by wxWidgets.
   //
   // TODO:  The whole Language initialization really need to be reworked.
   //        It's all over the place.
#if !wxCHECK_VERSION(3, 0, 1)
   for (size_t i = 0, cnt = WXSIZEOF(userLangs); i < cnt; i++)
   {
      wxLocale::AddLanguage(userLangs[i]);
   }
#endif

   // Initialize preferences and language
   InitPreferences();

#if defined(__WXMSW__) && !defined(__WXUNIVERSAL__) && !defined(__CYGWIN__)
   this->AssociateFileTypes();
#endif

   // TODO - read the number of files to store in history from preferences
   mRecentFiles = std::make_unique<FileHistory>(ID_RECENT_LAST - ID_RECENT_FIRST + 1, ID_RECENT_CLEAR);
   mRecentFiles->Load(*gPrefs, wxT("RecentFiles"));

   theTheme.EnsureInitialised();

   // AColor depends on theTheme.
   AColor::Init();

   // Init DirManager, which initializes the temp directory
   // If this fails, we must exit the program.
   if (!InitTempDir()) {
      FinishPreferences();
      return false;
   }

   //<<<< Try to avoid dialogs before this point.
   // The reason is that InitTempDir starts the single instance checker.
   // If we're waiitng in a dialog before then we can very easily
   // start multiple instances, defeating the single instance checker.

   // Initialize the CommandHandler
   InitCommandHandler();

   // Initialize the PluginManager
   PluginManager::Get().Initialize();

   // Initialize the ModuleManager, including loading found modules
   ModuleManager::Get().Initialize(*mCmdHandler);

   // Parse command line and handle options that might require
   // immediate exit...no need to initialize all of the audio
   // stuff to display the version string.
   auto parser = ParseCommandLine();
   if (!parser)
   {
      // Either user requested help or a parsing error occured
      exit(1);
   }

   if (parser->Found(wxT("v")))
   {
      wxFprintf(stderr, wxT("Audacity v%s\n"), AUDACITY_VERSION_STRING);
      exit(0);
   }

   long lval;
   if (parser->Found(wxT("b"), &lval))
   {
      if (lval < 256 || lval > 100000000)
      {
         wxPrintf(_("Block size must be within 256 to 100000000\n"));
         exit(1);
      }

      Sequence::SetMaxDiskBlockSize(lval);
   }

   wxString fileName;
   if (parser->Found(wxT("d"), &fileName))
   {
      AutoSaveFile asf;
      if (asf.Decode(fileName))
      {
         wxPrintf(_("File decoded successfully\n"));
      }
      else
      {
         wxPrintf(_("Decoding failed\n"));
      }
      exit(1);
   }

   // BG: Create a temporary window to set as the top window
   wxImage logoimage((const char **)AudacityLogoWithName_xpm);
   logoimage.Rescale(logoimage.GetWidth() / 2, logoimage.GetHeight() / 2);
   wxBitmap logo(logoimage);

   AudacityProject *project;
   {
      wxSplashScreen temporarywindow(
         logo,
         wxSPLASH_CENTRE_ON_SCREEN | wxSPLASH_NO_TIMEOUT,
         0,
         NULL,
         wxID_ANY,
         wxDefaultPosition,
         wxDefaultSize,
         wxSTAY_ON_TOP);
      temporarywindow.SetTitle(_("Audacity is starting up..."));
      SetTopWindow(&temporarywindow);

      // ANSWER-ME: Why is YieldFor needed at all?
      //wxEventLoopBase::GetActive()->YieldFor(wxEVT_CATEGORY_UI|wxEVT_CATEGORY_USER_INPUT|wxEVT_CATEGORY_UNKNOWN);
      wxEventLoopBase::GetActive()->YieldFor(wxEVT_CATEGORY_UI);

      //JKC: Would like to put module loading here.

      // More initialization

      InitDitherers();
      InitAudioIO();

#ifdef __WXMAC__

      // On the Mac, users don't expect a program to quit when you close the last window.
      // Create a menubar that will show when all project windows are closed.

      auto fileMenu = std::make_unique<wxMenu>();
      auto urecentMenu = std::make_unique<wxMenu>();
      auto recentMenu = urecentMenu.get();
      fileMenu->Append(wxID_NEW, wxString(_("&New")) + wxT("\tCtrl+N"));
      fileMenu->Append(wxID_OPEN, wxString(_("&Open...")) + wxT("\tCtrl+O"));
      fileMenu->AppendSubMenu(urecentMenu.release(), _("Open &Recent..."));
      fileMenu->Append(wxID_ABOUT, _("&About Audacity..."));
      fileMenu->Append(wxID_PREFERENCES, wxString(_("&Preferences...")) + wxT("\tCtrl+,"));

      {
         auto menuBar = std::make_unique<wxMenuBar>();
         menuBar->Append(fileMenu.release(), _("&File"));

         // PRL:  Are we sure wxWindows will not leak this menuBar?
         // The online documentation is not explicit.
         wxMenuBar::MacSetCommonMenuBar(menuBar.release());
      }

      mRecentFiles->UseMenu(recentMenu);
      mRecentFiles->AddFilesToMenu(recentMenu);

      SetExitOnFrameDelete(false);

#endif //__WXMAC__
      temporarywindow.Show(false);
   }

   // Workaround Bug 1377 - Crash after Audacity starts and low disk space warning appears
   // The temporary splash window is closed AND cleaned up, before attempting to create
   // a project and possibly creating a modal warning dialog by doing so.
   // Also fixes problem of warning being obscured.
   // Downside is that we have no splash screen for the (brief) time that we spend
   // creating the project.
   // Root cause is problem with wxSplashScreen and other dialogs co-existing, that
   // seemed to arrive with wx3.
   {
      project = CreateNewAudacityProject();
      mCmdHandler->SetProject(project);
      wxWindow * pWnd = MakeHijackPanel();
      if (pWnd)
      {
         project->Show(false);
         pWnd->SetParent(project);
         SetTopWindow(pWnd);
         pWnd->Show(true);
      }
   }

   if( project->mShowSplashScreen )
      project->OnHelpWelcome();

   // JKC 10-Sep-2007: Enable monitoring from the start.
   // (recommended by lprod.org).
   // Monitoring stops again after any
   // PLAY or RECORD completes.
   // So we also call StartMonitoring when STOP is called.
   project->MayStartMonitoring();

   #ifdef USE_FFMPEG
   FFmpegStartup();
   #endif

   Importer::Get().Initialize();

   //
   // Auto-recovery
   //
   bool didRecoverAnything = false;
   if (!ShowAutoRecoveryDialogIfNeeded(&project, &didRecoverAnything))
   {
      // Important: Prevent deleting any temporary files!
      DirManager::SetDontDeleteTempFiles();
      QuitAudacity(true);
      return false;
   }

   //
   // Remainder of command line parsing, but only if we didn't recover
   //
   if (!didRecoverAnything)
   {
      if (parser->Found(wxT("t")))
      {
         RunBenchmark(NULL);
         return false;
      }

// As of wx3, there's no need to process the filename arguments as they
// will be sent view the MacOpenFile() method.
#if !defined(__WXMAC__)
      for (size_t i = 0, cnt = parser->GetParamCount(); i < cnt; i++)
      {
         MRUOpen(parser->GetParam(i));
      }
#endif
   }

   gInited = true;

   ModuleManager::Get().Dispatch(AppInitialized);

   mWindowRectAlreadySaved = FALSE;

   mTimer.SetOwner(this, kAudacityAppTimerID);
   mTimer.Start(200);

   return TRUE;
}

void AudacityApp::InitCommandHandler()
{
   mCmdHandler = std::make_unique<CommandHandler>(*this);
   //SetNextHandler(mCmdHandler);
}

// AppCommandEvent callback - just pass the event on to the CommandHandler
void AudacityApp::OnReceiveCommand(AppCommandEvent &event)
{
   wxASSERT(NULL != mCmdHandler);
   mCmdHandler->OnReceiveCommand(event);
}

void AudacityApp::OnKeyDown(wxKeyEvent &event)
{
   if(event.GetKeyCode() == WXK_ESCAPE) {
      // Stop play, including scrub, but not record
      auto project = ::GetActiveProject();
      auto token = project->GetAudioIOToken();
      auto &scrubber = project->GetScrubber();
      auto scrubbing = scrubber.HasStartedScrubbing();
      if (scrubbing)
         scrubber.Cancel();
      if((token > 0 &&
               gAudioIO->IsAudioTokenActive(token) &&
               gAudioIO->GetNumCaptureChannels() == 0) ||
         scrubbing)
         // ESC out of other play (but not record)
         project->OnStop();
      else
         event.Skip();
   }
   else
      event.Skip();
}

// We now disallow temp directory name that puts it where cleaner apps will
// try to clean out the files.  
bool AudacityApp::IsTempDirectoryNameOK( const wxString & Name ){
   wxFileName tmpFile;
   tmpFile.AssignTempFileName(wxT("nn"));
   // use Long Path to expand out any abbreviated long substrings.
   wxString BadPath = tmpFile.GetLongPath();
   ::wxRemoveFile(tmpFile.GetFullPath());

#ifdef __WXMAC__
   // This test is to fix bug 1220 on a 1.x to 2.x to 2.1.3 upgrade.
   // It is less permissive than we could be as it stops a path
   // with this string ANYWHERE within it rather than excluding just
   // the paths that the earlier Audacities used to create.
   if( Name.Contains( "/tmp/") )
      return false;
   BadPath = BadPath.BeforeLast( '/' ) + "/";
   wxFileName cmpFile( Name );
   wxString NameCanonical = cmpFile.GetLongPath( ) + "/";
#else
   BadPath = BadPath.BeforeLast( '\\' ) + "\\";
   wxFileName cmpFile( Name );
   wxString NameCanonical = cmpFile.GetLongPath( ) + "\\";
#endif
   return !(NameCanonical.StartsWith( BadPath ));
}

bool AudacityApp::InitTempDir()
{
   // We need to find a temp directory location.

   wxString tempFromPrefs = gPrefs->Read(wxT("/Directories/TempDir"), wxT(""));
   wxString tempDefaultLoc = wxGetApp().defaultTempDir;

   wxString temp = wxT("");

   #ifdef __WXGTK__
   if (tempFromPrefs.Length() > 0 && tempFromPrefs[0] != wxT('/'))
      tempFromPrefs = wxT("");
   #endif

   // Stop wxWidgets from printing its own error messages

   wxLogNull logNo;

   // Try temp dir that was stored in prefs first
   if( !IsTempDirectoryNameOK( tempFromPrefs ) ){
      ;// Bad name?  Don't try and use it.
   } else if (tempFromPrefs != wxT("")) {
      if (wxDirExists(tempFromPrefs))
         temp = tempFromPrefs;
      else if (wxMkdir(tempFromPrefs, 0755))
         temp = tempFromPrefs;
   }

   // If that didn't work, try the default location

   if (temp==wxT("") && tempDefaultLoc != wxT("")) {
      if (wxDirExists(tempDefaultLoc))
         temp = tempDefaultLoc;
      else if (wxMkdir(tempDefaultLoc, 0755))
         temp = tempDefaultLoc;
   }

   // Check temp directory ownership on *nix systems only
   #ifdef __UNIX__
   struct stat tempStatBuf;
   if ( lstat(temp.mb_str(), &tempStatBuf) != 0 ) {
      temp.clear();
   }
   else {
      if ( geteuid() != tempStatBuf.st_uid ) {
         temp.clear();
      }
   }
   #endif

   if (temp == wxT("")) {
      // Failed
      if( !IsTempDirectoryNameOK( tempFromPrefs ) ) {
         wxMessageBox(_("Audacity could not find a safe place to store temporary files.\nAudacity needs a place where automatic cleanup programs won't delete the temporary files.\nPlease enter an appropriate directory in the preferences dialog."));
      } else {
         wxMessageBox(_("Audacity could not find a place to store temporary files.\nPlease enter an appropriate directory in the preferences dialog."));
      }

      // Only want one page of the preferences
      DirectoriesPrefsFactory directoriesPrefsFactory;
      PrefsDialog::Factories factories;
      factories.push_back(&directoriesPrefsFactory);
      GlobalPrefsDialog dialog(NULL, factories);
      dialog.ShowModal();

      wxMessageBox(_("Audacity is now going to exit. Please launch Audacity again to use the new temporary directory."));
      return false;
   }

   // The permissions don't always seem to be set on
   // some platforms.  Hopefully this fixes it...
   #ifdef __UNIX__
   chmod(OSFILENAME(temp), 0755);
   #endif

   bool bSuccess = gPrefs->Write(wxT("/Directories/TempDir"), temp) && gPrefs->Flush();
   DirManager::SetTempDir(temp);

   // Make sure the temp dir isn't locked by another process.
   if (!CreateSingleInstanceChecker(temp))
      return false;

   return bSuccess;
}

// Return true if there are no other instances of Audacity running,
// false otherwise.
//
// Use "dir" for creating lockfiles (on OS X and Unix).

bool AudacityApp::CreateSingleInstanceChecker(const wxString &dir)
{
   wxString name = wxString::Format(wxT("audacity-lock-%s"), wxGetUserId().c_str());
   mChecker.reset();
   auto checker = std::make_unique<wxSingleInstanceChecker>();

#if defined(__UNIX__)
   wxString sockFile(dir + wxT("/.audacity.sock"));
#endif

   wxString runningTwoCopiesStr = _("Running two copies of Audacity simultaneously may cause\ndata loss or cause your system to crash.\n\n");

   if (!checker->Create(name, dir)) {
      // Error initializing the wxSingleInstanceChecker.  We don't know
      // whether there is another instance running or not.

      wxString prompt =
         _("Audacity was not able to lock the temporary files directory.\nThis folder may be in use by another copy of Audacity.\n") +
         runningTwoCopiesStr +
         _("Do you still want to start Audacity?");
      int action = wxMessageBox(prompt,
                                _("Error Locking Temporary Folder"),
                                wxYES_NO | wxICON_EXCLAMATION,
                                NULL);
      if (action == wxNO)
         return false;
   }
   else if ( checker->IsAnotherRunning() ) {
      // Parse the command line to ensure correct syntax, but
      // ignore options and only use the filenames, if any.
      auto parser = ParseCommandLine();
      if (!parser)
      {
         // Complaints have already been made
         return false;
      }

#if defined(__WXMSW__)
      // On Windows, we attempt to make a connection
      // to an already active Audacity.  If successful, we send
      // the first command line argument (the audio file name)
      // to that Audacity for processing.
      wxClient client;

      // We try up to 50 times since there's a small window
      // where the server may not have been fully initialized.
      for (int i = 0; i < 50; i++)
      {
         std::unique_ptr<wxConnectionBase> conn{ client.MakeConnection(wxEmptyString, IPC_APPL, IPC_TOPIC) };
         if (conn)
         {
            bool ok = false;
            if (parser->GetParamCount() > 0)
            {
               // Send each parameter to existing Audacity
               for (size_t i = 0, cnt = parser->GetParamCount(); i < cnt; i++)
               {
                  ok = conn->Execute(parser->GetParam(i));
               }
             }
            else
            {
               // Send an empty string to force existing Audacity to front
               ok = conn->Execute(wxEmptyString);
            }

            if (ok)
               return false;
         }

         wxMilliSleep(10);
      }
#else
      // On Unix-like machines, we use a local (file based) socket to
      // send the first command line argument to an already running
      // Audacity.
      wxUNIXaddress addr;
      addr.Filename(sockFile);

      {
         // Setup the socket
         // A wxSocketClient must not be deleted by us, but rather, let the
         // framework do appropriate delayed deletion after Destroy()
         Destroy_ptr<wxSocketClient> sock { safenew wxSocketClient() };
         sock->SetFlags(wxSOCKET_WAITALL);

         // We try up to 50 times since there's a small window
         // where the server may not have been fully initialized.
         for (int i = 0; i < 50; i++)
         {
            // Connect to the existing Audacity
            sock->Connect(addr, true);
            if (sock->IsConnected())
            {
               for (size_t i = 0, cnt = parser->GetParamCount(); i < cnt; i++)
               {
                  // Send the filename
                  wxString param = parser->GetParam(i);
                  sock->WriteMsg((const wxChar *) param.c_str(), (param.Len() + 1) * sizeof(wxChar));
               }

               return false;
            }

            wxMilliSleep(100);
         }
      }
#endif
      // There is another copy of Audacity running.  Force quit.

      wxString prompt =
         _("The system has detected that another copy of Audacity is running.\n") +
         runningTwoCopiesStr +
         _("Use the New or Open commands in the currently running Audacity\nprocess to open multiple projects simultaneously.\n");
      wxMessageBox(prompt, _("Audacity is already running"),
            wxOK | wxICON_ERROR);
      return false;
   }

#if defined(__WXMSW__)
   // Create the DDE IPC server
   mIPCServ = std::make_unique<IPCServ>(IPC_APPL);
#else
   int mask = umask(077);
   remove(OSFILENAME(sockFile));
   wxUNIXaddress addr;
   addr.Filename(sockFile);
   mIPCServ = std::make_unique<wxSocketServer>(addr, wxSOCKET_NOWAIT);
   umask(mask);

   if (!mIPCServ || !mIPCServ->IsOk())
   {
      // TODO:  Complain here
      return false;
   }

   mIPCServ->SetEventHandler(*this, ID_IPC_SERVER);
   mIPCServ->SetNotify(wxSOCKET_CONNECTION_FLAG);
   mIPCServ->Notify(true);
#endif
   mChecker = std::move(checker);
   return true;
}

#if defined(__UNIX__)
void AudacityApp::OnServerEvent(wxSocketEvent & /* evt */)
{
   wxSocketBase *sock;

   // Accept all pending connection requests
   do
   {
      sock = mIPCServ->Accept(false);
      if (sock)
      {
         // Setup the socket
         sock->SetEventHandler(*this, ID_IPC_SOCKET);
         sock->SetNotify(wxSOCKET_INPUT_FLAG | wxSOCKET_LOST_FLAG);
         sock->Notify(true);
      }
   } while (sock);
}

void AudacityApp::OnSocketEvent(wxSocketEvent & evt)
{
   wxSocketBase *sock = evt.GetSocket();

   if (evt.GetSocketEvent() == wxSOCKET_LOST)
   {
      sock->Destroy();
      return;
   }

   // Read the length of the filename and bail if we have a short read
   wxChar name[PATH_MAX];
   sock->ReadMsg(&name, sizeof(name));
   if (!sock->Error())
   {
      // Add the filename to the queue.  It will be opened by
      // the OnTimer() event when it is safe to do so.
      ofqueue.Add(name);
   }
}

#endif

std::unique_ptr<wxCmdLineParser> AudacityApp::ParseCommandLine()
{
   auto parser = std::make_unique<wxCmdLineParser>(argc, argv);
   if (!parser)
   {
      return nullptr;
   }

   /*i18n-hint: This controls the number of bytes that Audacity will
    *           use when writing files to the disk */
   parser->AddOption(wxT("b"), wxT("blocksize"), _("set max disk block size in bytes"),
                     wxCMD_LINE_VAL_NUMBER);

   /*i18n-hint: This decodes an autosave file */
   parser->AddOption(wxT("d"), wxT("decode"), _("decode an autosave file"),
                     wxCMD_LINE_VAL_STRING);

   /*i18n-hint: This displays a list of available options */
   parser->AddSwitch(wxT("h"), wxT("help"), _("this help message"),
                     wxCMD_LINE_OPTION_HELP);

   /*i18n-hint: This runs a set of automatic tests on Audacity itself */
   parser->AddSwitch(wxT("t"), wxT("test"), _("run self diagnostics"));

   /*i18n-hint: This displays the Audacity version */
   parser->AddSwitch(wxT("v"), wxT("version"), _("display Audacity version"));

   /*i18n-hint: This is a list of one or more files that Audacity
    *           should open upon startup */
   parser->AddParam(_("audio or project file name"),
                    wxCMD_LINE_VAL_STRING,
                    wxCMD_LINE_PARAM_MULTIPLE | wxCMD_LINE_PARAM_OPTIONAL);

   // Run the parser
   if (parser->Parse() == 0)
      return parser;

   return{};
}

// static
void AudacityApp::AddUniquePathToPathList(const wxString &pathArg,
                                          wxArrayString &pathList)
{
   wxFileName pathNorm = pathArg;
   pathNorm.Normalize();
   const wxString newpath{ pathNorm.GetFullPath() };

   for(unsigned int i=0; i<pathList.GetCount(); i++) {
      if (wxFileName(newpath) == wxFileName(pathList[i]))
         return;
   }

   pathList.Add(newpath);
}

// static
void AudacityApp::AddMultiPathsToPathList(const wxString &multiPathStringArg,
                                          wxArrayString &pathList)
{
   wxString multiPathString(multiPathStringArg);
   while (multiPathString != wxT("")) {
      wxString onePath = multiPathString.BeforeFirst(wxPATH_SEP[0]);
      multiPathString = multiPathString.AfterFirst(wxPATH_SEP[0]);
      AddUniquePathToPathList(onePath, pathList);
   }
}

// static
void AudacityApp::FindFilesInPathList(const wxString & pattern,
                                      const wxArrayString & pathList,
                                      wxArrayString & results,
                                      int flags)
{
   wxLogNull nolog;

   if (pattern == wxT("")) {
      return;
   }

   wxFileName ff;

   for(size_t i = 0; i < pathList.GetCount(); i++) {
      ff = pathList[i] + wxFILE_SEP_PATH + pattern;
      wxDir::GetAllFiles(ff.GetPath(), &results, ff.GetFullName(), flags);
   }
}

void AudacityApp::OnEndSession(wxCloseEvent & event)
{
   bool force = !event.CanVeto();

   // Try to close each open window.  If the user hits Cancel
   // in a Save Changes dialog, don't continue.
   if (!gAudacityProjects.empty()) {
      while (gAudacityProjects.size()) {
         // Closing the project has side-effect of
         // deletion from gAudacityProjects
         if (force) {
            gAudacityProjects[0]->Close(true);
         }
         else if (!gAudacityProjects[0]->Close()) {
            gIsQuitting = false;
            event.Veto();
            break;
         }
      }
   }
}

void AudacityApp::AddFileToHistory(const wxString & name)
{
   mRecentFiles->AddFileToHistory(name);
}

int AudacityApp::OnExit()
{
   gIsQuitting = true;
   while(Pending())
   {
      Dispatch();
   }

   Importer::Get().Terminate();

   if(gPrefs)
   {
      bool bFalse = false;
      //Should we change the commands.cfg location next startup?
      if(gPrefs->Read(wxT("/QDeleteCmdCfgLocation"), &bFalse))
      {
         gPrefs->DeleteEntry(wxT("/QDeleteCmdCfgLocation"));
         gPrefs->Write(wxT("/DeleteCmdCfgLocation"), true);
         gPrefs->Flush();
      }
   }

   mRecentFiles->Save(*gPrefs, wxT("RecentFiles"));

   FinishPreferences();

#ifdef USE_FFMPEG
   DropFFmpegLibs();
#endif

   DeinitFFT();

   DeinitAudioIO();

   // Terminate the PluginManager (must be done before deleting the locale)
   PluginManager::Get().Terminate();

   if (mIPCServ)
   {
#if defined(__UNIX__)
      wxUNIXaddress addr;
      if (mIPCServ->GetLocal(addr))
      {
         remove(OSFILENAME(addr.Filename()));
      }
#endif
   }

   return 0;
}

// The following five methods are currently only used on Mac OS,
// where it's possible to have a menu bar but no windows open.
// It doesn't hurt any other platforms, though.

// ...That is, as long as you check to see if no windows are open
// before executing the stuff.
// To fix this, check to see how many project windows are open,
// and skip the event unless none are open (which should only happen
// on the Mac, at least currently.)

void AudacityApp::OnMenuAbout(wxCommandEvent & /*event*/)
{
   // This function shadows a similar function
   // in Menus.cpp, but should only be used on the Mac platform.
#ifdef __WXMAC__
   // Modeless dialog, consistent with other Mac applications
   // Not more than one at once!
   const auto instance = AboutDialog::ActiveIntance();
   if (instance)
      instance->Raise();
   else
      // This dialog deletes itself when dismissed
      (safenew AboutDialog{ nullptr })->Show(true);
#else
      wxASSERT(false);
#endif
}

void AudacityApp::OnMenuNew(wxCommandEvent & event)
{
   // This function shadows a similar function
   // in Menus.cpp, but should only be used on the Mac platform
   // when no project windows are open. This check assures that
   // this happens, and enable the same code to be present on
   // all platforms.

   if(gAudacityProjects.size() == 0)
      CreateNewAudacityProject();
   else
      event.Skip();
}


void AudacityApp::OnMenuOpen(wxCommandEvent & event)
{
   // This function shadows a similar function
   // in Menus.cpp, but should only be used on the Mac platform
   // when no project windows are open. This check assures that
   // this happens, and enable the same code to be present on
   // all platforms.


   if(gAudacityProjects.size() == 0)
      AudacityProject::OpenFiles(NULL);
   else
      event.Skip();


}

void AudacityApp::OnMenuPreferences(wxCommandEvent & event)
{
   // This function shadows a similar function
   // in Menus.cpp, but should only be used on the Mac platform
   // when no project windows are open. This check assures that
   // this happens, and enable the same code to be present on
   // all platforms.

   if(gAudacityProjects.size() == 0) {
      GlobalPrefsDialog dialog(NULL /* parent */ );
      dialog.ShowModal();
   }
   else
      event.Skip();

}

void AudacityApp::OnMenuExit(wxCommandEvent & event)
{
   // This function shadows a similar function
   // in Menus.cpp, but should only be used on the Mac platform
   // when no project windows are open. This check assures that
   // this happens, and enable the same code to be present on
   // all platforms.

   // LL:  Removed "if" to allow closing based on final project count.
   // if(gAudacityProjects.size() == 0)
      QuitAudacity();

   // LL:  Veto quit if projects are still open.  This can happen
   //      if the user selected Cancel in a Save dialog.
   event.Skip(gAudacityProjects.size() == 0);

}

//BG: On Windows, associate the aup file type with Audacity
/* We do this in the Windows installer now,
   to avoid issues where user doesn't have admin privileges, but
   in case that didn't work, allow the user to decide at startup.

   //v Should encapsulate this & allow access from Prefs, too,
   //      if people want to manually change associations.
*/
#if defined(__WXMSW__) && !defined(__WXUNIVERSAL__) && !defined(__CYGWIN__)
void AudacityApp::AssociateFileTypes()
{
   wxRegKey associateFileTypes;
   associateFileTypes.SetName(wxT("HKCR\\.AUP"));
   bool bKeyExists = associateFileTypes.Exists();
   if (!bKeyExists) {
      // Not at HKEY_CLASSES_ROOT. Try HKEY_CURRENT_USER.
      associateFileTypes.SetName(wxT("HKCU\\Software\\Classes\\.AUP"));
      bKeyExists = associateFileTypes.Exists();
   }
   if (!bKeyExists) {
      // File types are not currently associated.
      // Check pref in case user has already decided against it.
      bool bWantAssociateFiles = true;
      if (!gPrefs->Read(wxT("/WantAssociateFiles"), &bWantAssociateFiles) ||
            bWantAssociateFiles) {
         // Either there's no pref or user does want associations
         // and they got stepped on, so ask.
         int wantAssoc =
            wxMessageBox(
               _("Audacity project (.AUP) files are not currently \nassociated with Audacity. \n\nAssociate them, so they open on double-click?"),
               _("Audacity Project Files"),
               wxYES_NO | wxICON_QUESTION);
         if (wantAssoc == wxYES) {
            gPrefs->Write(wxT("/WantAssociateFiles"), true);
            gPrefs->Flush();

            wxString root_key;

            root_key = wxT("HKCU\\Software\\Classes\\");
            associateFileTypes.SetName(root_key + wxT(".AUP")); // Start again with HKEY_CLASSES_ROOT.
            if (!associateFileTypes.Create(true)) {
               // Not at HKEY_CLASSES_USER. Try HKEY_CURRENT_ROOT.
               root_key = wxT("HKCR\\");
               associateFileTypes.SetName(root_key + wxT(".AUP"));
               if (!associateFileTypes.Create(true)) {
                  // Actually, can't create keys. Empty root_key to flag failure.
                  root_key.Empty();
               }
            }
            if (root_key.IsEmpty()) {
               //v Warn that we can't set keys. Ask whether to set pref for no retry?
            } else {
               associateFileTypes = wxT("Audacity.Project"); // Finally set value for .AUP key

               associateFileTypes.SetName(root_key + wxT("Audacity.Project"));
               if(!associateFileTypes.Exists()) {
                  associateFileTypes.Create(true);
                  associateFileTypes = wxT("Audacity Project File");
               }

               associateFileTypes.SetName(root_key + wxT("Audacity.Project\\shell"));
               if(!associateFileTypes.Exists()) {
                  associateFileTypes.Create(true);
                  associateFileTypes = wxT("");
               }

               associateFileTypes.SetName(root_key + wxT("Audacity.Project\\shell\\open"));
               if(!associateFileTypes.Exists()) {
                  associateFileTypes.Create(true);
               }

               associateFileTypes.SetName(root_key + wxT("Audacity.Project\\shell\\open\\command"));
               wxString tmpRegAudPath;
               if(associateFileTypes.Exists()) {
                  tmpRegAudPath = wxString(associateFileTypes).Lower();
               }
               if (!associateFileTypes.Exists() ||
                     (tmpRegAudPath.Find(wxT("audacity.exe")) >= 0)) {
                  associateFileTypes.Create(true);
                  associateFileTypes = (wxString)argv[0] + (wxString)wxT(" \"%1\"");
               }

#if 0
               // These can be use later to support more startup messages
               // like maybe "Import into existing project" or some such.
               // Leaving here for an example...
               associateFileTypes.SetName(root_key + wxT("Audacity.Project\\shell\\open\\ddeexec"));
               if(!associateFileTypes.Exists()) {
                  associateFileTypes.Create(true);
                  associateFileTypes = wxT("%1");
               }

               associateFileTypes.SetName(root_key + wxT("Audacity.Project\\shell\\open\\ddeexec\\Application"));
               if(!associateFileTypes.Exists()) {
                  associateFileTypes.Create(true);
                  associateFileTypes = IPC_APPL;
               }

               associateFileTypes.SetName(root_key + wxT("Audacity.Project\\shell\\open\\ddeexec\\Topic"));
               if(!associateFileTypes.Exists()) {
                  associateFileTypes.Create(true);
                  associateFileTypes = IPC_TOPIC;
               }
#endif
            }
         } else {
            // User said no. Set a pref so we don't keep asking.
            gPrefs->Write(wxT("/WantAssociateFiles"), false);
            gPrefs->Flush();
         }
      }
   }
}
#endif

