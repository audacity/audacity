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

#include "AudacityApp.h"

#include "AudacityLogger.h"
#include "AboutDialog.h"
#include "AColor.h"
#include "AudioIO.h"
#include "Benchmark.h"
#include "DirManager.h"
#include "commands/CommandHandler.h"
#include "commands/AppCommandEvent.h"
#include "effects/LoadEffects.h"
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

//temporarilly commented out till it is added to all projects
//#include "Profiler.h"

#include "ModuleManager.h"

#include "import/Import.h"

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

// These lines ensure that Audacity gets WindowsXP themes.
// Without them we get the old-style Windows98/2000 look under XP.
#  if !defined(__WXWINCE__)
#     pragma comment(linker, "\"/manifestdependency:type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='X86' publicKeyToken='6595b64144ccf1df'\"")
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

#  if defined(USE_LIBRESAMPLE)
#     pragma comment(lib, "libresample")
#  endif

#  if defined(USE_LIBSAMPLERATE)
#     pragma comment(lib, "libsamplerate")
#  endif

#  if defined(USE_LIBSOXR)
#     pragma comment(lib, "libsoxr")
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

#endif //(__WXMSW__)

#include "../images/AudacityLogoWithName.xpm"

////////////////////////////////////////////////////////////
/// Custom events
////////////////////////////////////////////////////////////

DEFINE_EVENT_TYPE(EVT_OPEN_AUDIO_FILE);
DEFINE_EVENT_TYPE(EVT_CAPTURE_KEYBOARD);
DEFINE_EVENT_TYPE(EVT_RELEASE_KEYBOARD);
DEFINE_EVENT_TYPE(EVT_CAPTURE_KEY);

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

static wxFrame *gParentFrame = NULL;

static bool gInited = false;
bool gIsQuitting = false;

void QuitAudacity(bool bForce)
{
   if (gIsQuitting)
      return;

   gIsQuitting = true;

   // Try to close each open window.  If the user hits Cancel
   // in a Save Changes dialog, don't continue.
   // BG: unless force is true

   // BG: Are there any projects open?
   //-   if (!gAudacityProjects.IsEmpty())
/*start+*/
   if (gAudacityProjects.IsEmpty())
   {
#ifdef __WXMAC__
      AudacityProject::DeleteClipboard();
#endif
   }
   else
/*end+*/
   {
      SaveWindowSize();
      while (gAudacityProjects.Count())
      {
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

   LWSlider::DeleteSharedTipPanel();

   ModuleManager::Get().Dispatch(AppQuiting);

   if (gParentFrame)
      gParentFrame->Destroy();
   gParentFrame = NULL;

   CloseContrastDialog();
#ifdef EXPERIMENTAL_SCOREALIGN
   CloseScoreAlignDialog();
#endif
   CloseScreenshotTools();

   //release ODManager Threads
   ODManager::Quit();

   //print out profile if we have one by deleting it
   //temporarilly commented out till it is added to all projects
   //delete Profiler::Instance();

   //delete the static lock for audacity projects
   AudacityProject::DeleteAllProjectsDeleteLock();

   //remove our logger
   delete wxLog::SetActiveTarget(NULL);

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
   size_t numProjects = gAudacityProjects.Count();
   for (size_t i = 0; i < numProjects; i++)
   {
      if (!gAudacityProjects[i]->IsIconized()) {
         validWindowForSaveWindowSize = TRUE;
         validProject = gAudacityProjects[i];
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
         validProject = gAudacityProjects[0];
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

static void interact_cb(GnomeClient *client,
                        gint key,
                        GnomeDialogType type,
                        gpointer data)
{
   wxCloseEvent e(wxEVT_QUERY_END_SESSION, wxID_ANY);
   e.SetEventObject(&wxGetApp());
   e.SetCanVeto(true);

   wxGetApp().ProcessEvent(e);

   gnome_interaction_key_return(key, e.GetVeto());
}

static gboolean save_yourself_cb(GnomeClient *client,
                                 gint phase,
                                 GnomeSaveStyle style,
                                 gboolean shutdown,
                                 GnomeInteractStyle interact,
                                 gboolean fast,
                                 gpointer user_data)
{
   if (!shutdown || interact != GNOME_INTERACT_ANY) {
      return TRUE;
   }

   if (gAudacityProjects.IsEmpty()) {
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

class IPCConn : public wxConnection
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

#if !wxCHECK_VERSION(3, 0, 0)
   bool OnExecute(const wxString & topic,
                  wxChar *data,
                  int WXUNUSED(size),
                  wxIPCFormat WXUNUSED(format))
   {
      return OnExec(topic, data);
   }
#endif
};

class IPCServ : public wxServer
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

   wxConnectionBase *OnAcceptConnection(const wxString & topic)
   {
      if (topic != IPC_TOPIC) {
         return NULL;
      }

      return new IPCConn();
   };
};

#ifndef __WXMAC__
IMPLEMENT_APP(AudacityApp)
/* make the application class known to wxWidgets for dynamic construction */
#endif

#ifdef __WXMAC__
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
   return wxEntry(argc, argv);
}
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

   if (gAudacityProjects.GetCount() == 0) {
      CreateNewAudacityProject();
   }
}

#endif //__WXMAC__

typedef int (AudacityApp::*SPECIALKEYEVENT)(wxKeyEvent&);

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

   EVT_KEY_DOWN(AudacityApp::OnKeyDown)
   EVT_CHAR(AudacityApp::OnChar)
   EVT_KEY_UP(AudacityApp::OnKeyUp)
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
END_EVENT_TABLE()

// backend for OnMRUFile
// TODO: Would be nice to make this handle not opening a file with more panache.
//  - Inform the user if DefaultOpenPath not set.
//  - Switch focus to correct instance of project window, if already open.
bool AudacityApp::MRUOpen(wxString fullPathStr) {
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
         // all, and it's not safe to open a new project directly in its
         // place.  Only if the project is brand-new clean and the user
         // hasn't done any action at all is it safe for Open to take place
         // inside the current project.
         //
         // If you try to Open a new project inside the current window when
         // there are no tracks, but there's an Undo history, etc, then
         // bad things can happen, including data files moving to the new
         // project directory, etc.
         if (!proj || proj->GetDirty() || !proj->GetIsEmpty()) {
            proj = CreateNewAudacityProject();
         }
         // This project is clean; it's never been touched.  Therefore
         // all relevant member variables are in their initial state,
         // and it's okay to open a new project inside this window.
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
   wxString fullPathStr = mRecentFiles->GetHistoryFile(n);

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
            wxString name(ofqueue[0]);
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
      size_t numProjects = gAudacityProjects.Count();
      wxString missingFileName;
      AudacityProject *offendingProject = NULL;

      m_LastMissingBlockFileLock.Lock();
      if (numProjects == 1) {
         // if there is only one project open, no need to search
         offendingProject = gAudacityProjects[0];
      } else if (numProjects > 1) {
         for (size_t i = 0; i < numProjects; i++) {
            // search each project for the blockfile
            if (gAudacityProjects[i]->GetDirManager()->ContainsBlockFile(m_LastMissingBlockFile)) {
               offendingProject = gAudacityProjects[i];
               break;
            }
         }
      }
      missingFileName = ((AliasBlockFile*)m_LastMissingBlockFile)->GetAliasedFileName().GetFullPath();
      m_LastMissingBlockFileLock.Unlock();

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

void AudacityApp::MarkAliasedFilesMissingWarning(BlockFile *b)
{
   // the reference counting provides thread safety.
   if (b)
      b->Ref();

   m_LastMissingBlockFileLock.Lock();
   if (m_LastMissingBlockFile)
      m_LastMissingBlockFile->Deref();

   m_LastMissingBlockFile = b;

   m_LastMissingBlockFileLock.Unlock();
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
      MarkAliasedFilesMissingWarning(NULL);
   }
}

bool AudacityApp::ShouldShowMissingAliasedFileWarning()
{
   bool ret = m_LastMissingBlockFile && m_aliasMissingWarningShouldShow;

   return ret;
}

AudacityLogger *AudacityApp::GetLogger()
{
   return static_cast<AudacityLogger *>(wxLog::GetActiveTarget());
}

void AudacityApp::InitLang( const wxString & lang )
{
   if( mLocale )
      delete mLocale;

// LL: I do not know why loading translations fail on the Mac if LANG is not
//     set, but for some reason it does.  So wrap the creation of wxLocale
//     with the default translation.
//
//     2013-09-13:  I've checked this again and it is still required.  Still
//                  no idea why.
#if defined(__WXMAC__)
   wxString oldval;
   bool existed;

   existed = wxGetEnv(wxT("LANG"), &oldval);
   wxSetEnv(wxT("LANG"), wxT("en_US"));
#endif

   mLocale = new wxLocale(wxT(""), lang, wxT(""), true, true);

#if defined(__WXMAC__)
   if (existed) {
      wxSetEnv(wxT("LANG"), oldval);
   }
   else {
      wxUnsetEnv(wxT("LANG"));
   }
#endif

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
}

// Only used when checking plugins
void AudacityApp::OnFatalException()
{
   exit(-1);
}

#if defined(__WXGTK__)
// On wxGTK, there's a focus issue where dialogs do not automatically pass focus
// to the first child.  This means that you can use the keyboard to navigate within
// the dialog.  Watching for the ACTIVATE event allows us to set the focus ourselves
// when each dialog opens.
//
// See bug #57
//
int AudacityApp::FilterEvent(wxEvent & event)
{
   if (event.GetEventType() == wxEVT_ACTIVATE)
   {
      wxActivateEvent & e = (wxActivateEvent &) event;

      if (e.GetEventObject() && e.GetActive() && e.GetEventObject()->IsKindOf(CLASSINFO(wxDialog)))
      {
         ((wxWindow *)e.GetEventObject())->SetFocus();
      }
   }

   return -1;
}
#endif

// The `main program' equivalent, creating the windows and returning the
// main frame
bool AudacityApp::OnInit()
{
   delete wxLog::SetActiveTarget(new AudacityLogger);

   m_aliasMissingWarningShouldShow = true;
   m_LastMissingBlockFile = NULL;

   mChecker = NULL;
   mIPCServ = NULL;

#if defined(__WXGTK__)
   // Workaround for bug 154 -- initialize to false
   inKbdHandler = false;
#endif

#if defined(__WXMAC__)
   // Disable window animation
   wxSystemOptions::SetOption( wxMAC_WINDOW_PLAIN_TRANSITION, 1 );
#endif

#ifdef AUDACITY_NAME
   wxString appName = wxT(AUDACITY_NAME);
   wxString vendorName = wxT(AUDACITY_NAME);
#else
   wxString vendorName = wxT("Audacity");
   wxString appName = wxT("Audacity");
#endif

   wxTheApp->SetVendorName(vendorName);
   wxTheApp->SetAppName(appName);

   // Unused strings that we want to be translated, even though
   // we're not using them yet...
   wxString future1 = _("Master Gain Control");

   ::wxInitAllImageHandlers();

   wxFileSystem::AddHandler(new wxZipFSHandler);

   // Use the system language for dialogs that are displayed before
   // the user selected language is available from preferences.
   mLocale = NULL;
   InitLang(GetSystemLanguageCode());

   InitPreferences();

   #if defined(__WXMSW__) && !defined(__WXUNIVERSAL__) && !defined(__CYGWIN__)
      this->AssociateFileTypes();
   #endif

   // TODO - read the number of files to store in history from preferences
   mRecentFiles = new FileHistory(ID_RECENT_LAST - ID_RECENT_FIRST + 1, ID_RECENT_CLEAR);
   mRecentFiles->Load(*gPrefs, wxT("RecentFiles"));

   //
   // Paths: set search path and temp dir path
   //

   wxString home = wxGetHomeDir();
   theTheme.EnsureInitialised();

   // AColor depends on theTheme.
   AColor::Init();

   /* Search path (for plug-ins, translations etc) is (in this order):
      * The AUDACITY_PATH environment variable
      * The current directory
      * The user's .audacity-files directory in their home directory
      * The "share" and "share/doc" directories in their install path */
   #ifdef __WXGTK__
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

   wxFileName tmpFile;
   tmpFile.AssignTempFileName(wxT("nn"));
   wxString tmpDirLoc = tmpFile.GetPath(wxPATH_GET_VOLUME);
   ::wxRemoveFile(tmpFile.GetFullPath());

   // On Mac and Windows systems, use the directory which contains Audacity.
   #ifdef __WXMSW__
   // On Windows, the path to the Audacity program is in argv[0]
   wxString progPath = wxPathOnly(argv[0]);
   AddUniquePathToPathList(progPath, audacityPathList);
   AddUniquePathToPathList(progPath+wxT("\\Languages"), audacityPathList);

   defaultTempDir.Printf(wxT("%s\\audacity_temp"),
                         tmpDirLoc.c_str());
   #endif //__WXWSW__

   #ifdef __WXMAC__
   // On Mac OS X, the path to the Audacity program is in argv[0]
   wxString progPath = wxPathOnly(argv[0]);

   AddUniquePathToPathList(progPath, audacityPathList);
   // If Audacity is a "bundle" package, then the root directory is
   // the great-great-grandparent of the directory containing the executable.
   AddUniquePathToPathList(progPath+wxT("/../../../"), audacityPathList);

   // These allow for searching the "bundle"
   AddUniquePathToPathList(progPath+wxT("/../"), audacityPathList);
   AddUniquePathToPathList(progPath+wxT("/../Resources"), audacityPathList);

   defaultTempDir.Printf(wxT("%s/audacity-%s"),
                         tmpDirLoc.c_str(),
                         wxGetUserId().c_str());
   #endif //__WXMAC__

   // Reset the language now that translation paths and preferences are available

   wxString lang = gPrefs->Read(wxT("/Locale/Language"), wxT(""));

   if (lang == wxT(""))
      lang = GetSystemLanguageCode();

   InitLang( lang );

   // Init DirManager, which initializes the temp directory
   // If this fails, we must exit the program.

   if (!InitTempDir()) {
      FinishPreferences();
      return false;
   }

   //JKC We'd like to initialise the module manager WHILE showing the splash screen.
   //Can't in wx3.0.1 as MultiDialog interacts poorly with the splash screen.  So we do it before.
   //TODO: Find out why opening a multidialog wrecks the splash screen.
   //best current guess is that it's something to do with doing a DoModal this early
   //in the program.

   // Initialize the CommandHandler
   InitCommandHandler();

   // Initialize the PluginManager
   PluginManager::Get().Initialize();

   // Initialize the ModuleManager, including loading found modules
   ModuleManager::Get().Initialize(*mCmdHandler);

#if !wxCHECK_VERSION(3, 0, 0)
   FinishInits();
#endif

   return TRUE;
}

#if wxCHECK_VERSION(3, 0, 0)
#include <wx/evtloop.h>
static bool bInitsDone = false;
void AudacityApp::OnEventLoopEnter(wxEventLoopBase * pLoop)
{
   if( !pLoop->IsMain() )
      return;
   if (bInitsDone)
      return;
   bInitsDone = true;
   FinishInits();
}
#endif

// JKC: I've split 'FinishInits()' from 'OnInit()', so that 
// we can have a real event loop running.  We could (I think) 
// put everything that is in OnInit() in here.
// This change was to support wxWidgets 3.0.0 and allow us
// to show a dialog (for module loading) during initialisation.
// without it messing up the splash screen.
// Hasn't actually fixed that yet, but is addressing the point
// they make in their release notes.
void AudacityApp::FinishInits()
{
   // Parse command line and handle options that might require
   // immediate exit...no need to initialize all of the audio
   // stuff to display the version string.
   wxCmdLineParser *parser = ParseCommandLine();
   if (!parser)
   {
      delete parser;

      // Either user requested help or a parsing error occured
      exit(1);
   }

   if (parser->Found(wxT("v")))
   {
      delete parser;

      wxFprintf(stderr, wxT("Audacity v%s\n"), AUDACITY_VERSION_STRING);
      exit(0);
   }

   long lval;
   if (parser->Found(wxT("b"), &lval))
   {
      if (lval < 256 || lval > 100000000)
      {
         delete parser;

         wxPrintf(_("Block size must be within 256 to 100000000\n"));
         exit(1);
      }

      Sequence::SetMaxDiskBlockSize(lval);
   }

// No Splash screen on wx3 whislt we sort out the problem
// with showing a dialog AND a splash screen during inits.
#if !wxCHECK_VERSION(3, 0, 0)
   // BG: Create a temporary window to set as the top window
   wxImage logoimage((const char **) AudacityLogoWithName_xpm);
   logoimage.Rescale(logoimage.GetWidth() / 2, logoimage.GetHeight() / 2);
   wxBitmap logo(logoimage);

   wxSplashScreen *temporarywindow =
      new wxSplashScreen(logo,
                         wxSPLASH_CENTRE_ON_SCREEN | wxSPLASH_NO_TIMEOUT,
                         0,
                         NULL,
                         wxID_ANY,
                         wxDefaultPosition,
                         wxDefaultSize,
                         wxSTAY_ON_TOP);
   temporarywindow->SetTitle(_("Audacity is starting up..."));
   SetTopWindow(temporarywindow);
#endif

   //JKC: Would like to put module loading here.

   // More initialization

   InitDitherers();
   InitAudioIO();

   LoadEffects();

#ifdef __WXMAC__

   // On the Mac, users don't expect a program to quit when you close the last window.
   // Create a menubar that will show when all project windows are closed.

   wxMenu *fileMenu = new wxMenu();
   wxMenu *recentMenu = new wxMenu();
   fileMenu->Append(wxID_NEW, wxString(_("&New")) + wxT("\tCtrl+N"));
   fileMenu->Append(wxID_OPEN, wxString(_("&Open...")) + wxT("\tCtrl+O"));
   fileMenu->AppendSubMenu(recentMenu, _("Open &Recent..."));
   fileMenu->Append(wxID_ABOUT, _("&About Audacity..."));
   fileMenu->Append(wxID_PREFERENCES, wxString(_("&Preferences...")) + wxT("\tCtrl+,"));

   wxMenuBar *menuBar = new wxMenuBar();
   menuBar->Append(fileMenu, _("&File"));

   wxMenuBar::MacSetCommonMenuBar(menuBar);

   mRecentFiles->UseMenu(recentMenu);
   mRecentFiles->AddFilesToMenu(recentMenu);

   // This invisibale frame will be the "root" of all other frames and will
   // become the active frame when no projects are open.
   gParentFrame = new wxFrame(NULL, -1, wxEmptyString, wxPoint(0, 0), wxSize(0, 0), 0);

#endif //__WXMAC__

   SetExitOnFrameDelete(true);


   AudacityProject *project = CreateNewAudacityProject();
   mCmdHandler->SetProject(project);
   wxWindow * pWnd = MakeHijackPanel() ;
   if( pWnd )
   {
      project->Show( false );
      pWnd->SetParent( project );
      SetTopWindow(pWnd);
      pWnd->Show( true );
   }


#if !wxCHECK_VERSION(3, 0, 0)
   temporarywindow->Show(false);
   delete temporarywindow;
#endif

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
      delete parser;
      QuitAudacity(true);
      return;
   }

   //
   // Remainder of command line parsing, but only if we didn't recover
   //
   if (!didRecoverAnything)
   {
      if (parser->Found(wxT("t")))
      {
         delete parser;
   
         RunBenchmark(NULL);
         return;
      }

      for (size_t i = 0, cnt = parser->GetParamCount(); i < cnt; i++)
      {
         MRUOpen(parser->GetParam(i));
      }   
   }

   delete parser;

   gInited = true;

   ModuleManager::Get().Dispatch(AppInitialized);

   mWindowRectAlreadySaved = FALSE;

   mTimer.SetOwner(this, kAudacityAppTimerID);
   mTimer.Start(200);
}

void AudacityApp::InitCommandHandler()
{
   mCmdHandler = new CommandHandler(*this);
   //SetNextHandler(mCmdHandler);
}

void AudacityApp::DeInitCommandHandler()
{
   wxASSERT(NULL != mCmdHandler);
   delete mCmdHandler;
   mCmdHandler = NULL;
}

// AppCommandEvent callback - just pass the event on to the CommandHandler
void AudacityApp::OnReceiveCommand(AppCommandEvent &event)
{
   wxASSERT(NULL != mCmdHandler);
   mCmdHandler->OnReceiveCommand(event);
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

   if (tempFromPrefs != wxT("")) {
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
      wxMessageBox(_("Audacity could not find a place to store temporary files.\nPlease enter an appropriate directory in the preferences dialog."));

      PrefsDialog dialog(NULL);
      dialog.ShowTempDirPage();
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

bool AudacityApp::CreateSingleInstanceChecker(wxString dir)
{
   wxString name = wxString::Format(wxT("audacity-lock-%s"), wxGetUserId().c_str());
   mChecker = new wxSingleInstanceChecker();

#if defined(__UNIX__)
   wxString sockFile(FileNames::DataDir() + wxT("/.audacity.sock"));
#endif

   wxString runningTwoCopiesStr = _("Running two copies of Audacity simultaneously may cause\ndata loss or cause your system to crash.\n\n");

   if (!mChecker->Create(name, dir)) {
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
      if (action == wxNO) {
         delete mChecker;
         return false;
      }
   }
   else if ( mChecker->IsAnotherRunning() ) {
      // Parse the command line to ensure correct syntax, but
      // ignore options and only use the filenames, if any.
      wxCmdLineParser *parser = ParseCommandLine();
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
         wxConnectionBase *conn = client.MakeConnection(wxEmptyString, IPC_APPL, IPC_TOPIC);
         if (conn)
         {
            bool ok;
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

            delete conn;

            if (ok)
            {
               delete parser;
               return false;
            }
         }

         wxMilliSleep(10);
      }
#else
      // On Unix-like machines, we use a local (file based) socket to
      // send the first command line argument to an already running
      // Audacity.
      wxUNIXaddress addr;
      addr.Filename(sockFile);

      // Setup the socket
      wxSocketClient *sock = new wxSocketClient();
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

            sock->Destroy();
            delete parser;
            return false;
         }

         wxMilliSleep(100);
      }

      sock->Destroy();
#endif
      // There is another copy of Audacity running.  Force quit.

      wxString prompt =
         _("The system has detected that another copy of Audacity is running.\n") +
         runningTwoCopiesStr +
         _("Use the New or Open commands in the currently running Audacity\nprocess to open multiple projects simultaneously.\n");
      wxMessageBox(prompt, _("Audacity is already running"),
            wxOK | wxICON_ERROR);
      delete parser;
      delete mChecker;
      return false;
   }

#if defined(__WXMSW__)
   // Create the DDE IPC server
   mIPCServ = new IPCServ(IPC_APPL);
#else
   int mask = umask(077);
   remove(OSFILENAME(sockFile));
   wxUNIXaddress addr;
   addr.Filename(sockFile);
   mIPCServ = new wxSocketServer(addr, wxSOCKET_NOWAIT);
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
   return true;
}

#if defined(__UNIX__)
void AudacityApp::OnServerEvent(wxSocketEvent & evt)
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

wxCmdLineParser *AudacityApp::ParseCommandLine()
{
   wxCmdLineParser *parser = new wxCmdLineParser(argc, argv);
   if (!parser)
   {
      return NULL;
   }

   /*i18n-hint: This controls the number of bytes that Audacity will
    *           use when writing files to the disk */
   parser->AddOption(wxT("b"), wxT("blocksize"), _("set max disk block size in bytes"),
                     wxCMD_LINE_VAL_NUMBER);

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
   {
      return parser;
   }

   delete parser;

   return NULL;
}

// static
void AudacityApp::AddUniquePathToPathList(wxString path,
                                          wxArrayString &pathList)
{
   wxFileName pathNorm = path;
   pathNorm.Normalize();
   path = pathNorm.GetFullPath();

   for(unsigned int i=0; i<pathList.GetCount(); i++) {
      if (wxFileName(path) == wxFileName(pathList[i]))
         return;
   }

   pathList.Add(path);
}

// static
void AudacityApp::AddMultiPathsToPathList(wxString multiPathString,
                                          wxArrayString &pathList)
{
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

   wxFileName f;

   for(size_t i = 0; i < pathList.GetCount(); i++) {
      f = pathList[i] + wxFILE_SEP_PATH + pattern;
      wxDir::GetAllFiles(f.GetPath(), &results, f.GetFullName(), flags);
   }
}

void AudacityApp::OnEndSession(wxCloseEvent & event)
{
   bool force = !event.CanVeto();

   // Try to close each open window.  If the user hits Cancel
   // in a Save Changes dialog, don't continue.
   if (!gAudacityProjects.IsEmpty()) {
      while (gAudacityProjects.Count()) {
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

void AudacityApp::OnKeyDown(wxKeyEvent & event)
{
   // Not handled
   event.Skip(true);

   // Make sure this event is destined for a project window
   AudacityProject *prj = GetActiveProject();

   // TODO: I don't know how it can happen, but it did on 2006-07-06.
   // I was switching between apps fairly quickly so maybe that has something
   // to do with it.
   if (!prj)
      return;

   if (prj->HandleKeyDown(event))
      event.Skip(false);
}

void AudacityApp::OnChar(wxKeyEvent & event)
{
   // Not handled
   event.Skip(true);

   // Make sure this event is destined for a project window
   AudacityProject *prj = GetActiveProject();

   // TODO: I don't know how it can happen, but it did on 2006-07-06.
   // I was switching between apps fairly quickly so maybe that has something
   // to do with it.
   if (!prj)
      return;

   if (prj->HandleChar(event))
      event.Skip(false);
}

void AudacityApp::OnKeyUp(wxKeyEvent & event)
{
   // Not handled
   event.Skip(true);

   // Make sure this event is destined for a project window
   AudacityProject *prj = GetActiveProject();

   // TODO: I don't know how it can happen, but it did on 2006-07-06.
   // I was switching between apps fairly quickly so maybe that has something
   // to do with it.
   if (!prj)
      return;

   if (prj != wxGetTopLevelParent(wxWindow::FindFocus()))
      return;

   if (prj->HandleKeyUp(event))
      event.Skip(false);
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

   DeInitCommandHandler();

   mRecentFiles->Save(*gPrefs, wxT("RecentFiles"));
   delete mRecentFiles;

   FinishPreferences();

#ifdef USE_FFMPEG
   DropFFmpegLibs();
#endif

   UnloadEffects();

   DeinitFFT();
   BlockFile::Deinit();

   DeinitAudioIO();

   // Terminate the PluginManager (must be done before deleting the locale)
   PluginManager::Get().Terminate();

   // Done with plugins and modules
   PluginManager::Destroy();
   ModuleManager::Destroy();

   if (mLocale)
      delete mLocale;

   if (mIPCServ)
   {
#if defined(__UNIX__)
      wxUNIXaddress addr;
      if (mIPCServ->GetLocal(addr))
      {
         remove(OSFILENAME(addr.Filename()));
      }
#endif
      delete mIPCServ;
   }

   if (mChecker)
      delete mChecker;

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

void AudacityApp::OnMenuAbout(wxCommandEvent & event)
{
   // This function shadows a similar function
   // in Menus.cpp, but should only be used on the Mac platform
   // when no project windows are open. This check assures that
   // this happens, and enable the same code to be present on
   // all platforms.
   if(gAudacityProjects.GetCount() == 0) {
      AboutDialog dlog(NULL);
      dlog.ShowModal();
   }
   else
      event.Skip();
}

void AudacityApp::OnMenuNew(wxCommandEvent & event)
{
   // This function shadows a similar function
   // in Menus.cpp, but should only be used on the Mac platform
   // when no project windows are open. This check assures that
   // this happens, and enable the same code to be present on
   // all platforms.

   if(gAudacityProjects.GetCount() == 0)
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


   if(gAudacityProjects.GetCount() == 0)
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

   if(gAudacityProjects.GetCount() == 0) {
      PrefsDialog dialog(NULL /* parent */ );
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
   // if(gAudacityProjects.GetCount() == 0)
      QuitAudacity();

   // LL:  Veto quit if projects are still open.  This can happen
   //      if the user selected Cancel in a Save dialog.
   event.Skip(gAudacityProjects.GetCount() == 0);

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

