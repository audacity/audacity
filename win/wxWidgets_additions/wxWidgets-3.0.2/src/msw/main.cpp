/////////////////////////////////////////////////////////////////////////////
// Name:        src/msw/main.cpp
// Purpose:     WinMain/DllMain
// Author:      Julian Smart
// Modified by:
// Created:     04/01/98
// Copyright:   (c) Julian Smart
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

// ============================================================================
// declarations
// ============================================================================

// ----------------------------------------------------------------------------
// headers
// ----------------------------------------------------------------------------

// For compilers that support precompilation, includes "wx.h".
#include "wx/wxprec.h"

#ifdef __BORLANDC__
    #pragma hdrstop
#endif

#ifndef WX_PRECOMP
    #include "wx/event.h"
    #include "wx/app.h"
    #include "wx/utils.h"
#endif //WX_PRECOMP

#include "wx/cmdline.h"
#include "wx/dynlib.h"
#include "wx/scopeguard.h"

#include "wx/msw/private.h"
#include "wx/msw/seh.h"

#if wxUSE_ON_FATAL_EXCEPTION
    #include "wx/datetime.h"
    #include "wx/msw/crashrpt.h"
#endif // wxUSE_ON_FATAL_EXCEPTION

#ifdef __WXWINCE__
    // there is no ExitProcess() under CE but exiting the main thread has the
    // same effect
    #ifndef ExitProcess
        #define ExitProcess ExitThread
    #endif
#endif // __WXWINCE__

#ifdef __BORLANDC__
    // BC++ has to be special: its run-time expects the DLL entry point to be
    // named DllEntryPoint instead of the (more) standard DllMain
    #define DllMain DllEntryPoint
#endif // __BORLANDC__

#if defined(__WXMICROWIN__)
    #define HINSTANCE HANDLE
#endif

// defined in common/init.cpp
extern int wxEntryReal(int& argc, wxChar **argv);
extern int wxEntryCleanupReal(int& argc, wxChar **argv);

// ============================================================================
// implementation: various entry points
// ============================================================================

#if wxUSE_BASE

// ----------------------------------------------------------------------------
// wrapper wxEntry catching all Win32 exceptions occurring in a wx program
// ----------------------------------------------------------------------------

// wrap real wxEntry in a try-except block to be able to call
// OnFatalException() if necessary
#if wxUSE_ON_FATAL_EXCEPTION

// global pointer to exception information, only valid inside OnFatalException,
// used by wxStackWalker and wxCrashReport
extern EXCEPTION_POINTERS *wxGlobalSEInformation = NULL;

// flag telling us whether the application wants to handle exceptions at all
static bool gs_handleExceptions = false;

static void wxFatalExit()
{
    // use the same exit code as abort()
    ::ExitProcess(3);
}

unsigned long wxGlobalSEHandler(EXCEPTION_POINTERS *pExcPtrs)
{
    if ( gs_handleExceptions && wxTheApp )
    {
        // store the pointer to exception info
        wxGlobalSEInformation = pExcPtrs;

        // give the user a chance to do something special about this
        wxSEH_TRY
        {
            wxTheApp->OnFatalException();
        }
        wxSEH_IGNORE      // ignore any exceptions inside the exception handler

        wxGlobalSEInformation = NULL;

        // this will execute our handler and terminate the process
        return EXCEPTION_EXECUTE_HANDLER;
    }

    return EXCEPTION_CONTINUE_SEARCH;
}

#ifdef __VISUALC__

void wxSETranslator(unsigned int WXUNUSED(code), EXCEPTION_POINTERS *ep)
{
    switch ( wxGlobalSEHandler(ep) )
    {
        default:
            wxFAIL_MSG( wxT("unexpected wxGlobalSEHandler() return value") );
            // fall through

        case EXCEPTION_EXECUTE_HANDLER:
            // if wxApp::OnFatalException() had been called we should exit the
            // application -- but we shouldn't kill our host when we're a DLL
#ifndef WXMAKINGDLL
            wxFatalExit();
#endif // not a DLL
            break;

        case EXCEPTION_CONTINUE_SEARCH:
            // we're called for each "catch ( ... )" and if we (re)throw from
            // here, the catch handler body is not executed, so the effect is
            // as if had inhibited translation of SE to C++ ones because the
            // handler will never see any structured exceptions
            throw;
    }
}

#endif // __VISUALC__

bool wxHandleFatalExceptions(bool doit)
{
    // assume this can only be called from the main thread
    gs_handleExceptions = doit;

#if wxUSE_CRASHREPORT
    if ( doit )
    {
        // try to find a place where we can put out report file later
        wxChar fullname[MAX_PATH];
        if ( !::GetTempPath(WXSIZEOF(fullname), fullname) )
        {
            wxLogLastError(wxT("GetTempPath"));

            // when all else fails...
            wxStrcpy(fullname, wxT("c:\\"));
        }

        // use PID and date to make the report file name more unique
        wxString name = wxString::Format
                        (
                            wxT("%s_%s_%lu.dmp"),
                            wxTheApp ? (const wxChar*)wxTheApp->GetAppDisplayName().c_str()
                                     : wxT("wxwindows"),
                            wxDateTime::Now().Format(wxT("%Y%m%dT%H%M%S")).c_str(),
                            ::GetCurrentProcessId()
                        );

        wxStrncat(fullname, name, WXSIZEOF(fullname) - wxStrlen(fullname) - 1);

        wxCrashReport::SetFileName(fullname);
    }
#endif // wxUSE_CRASHREPORT

    return true;
}

int wxEntry(int& argc, wxChar **argv)
{
    DisableAutomaticSETranslator();

    wxSEH_TRY
    {
        return wxEntryReal(argc, argv);
    }
    wxSEH_HANDLE(-1)
}

#else // !wxUSE_ON_FATAL_EXCEPTION

int wxEntry(int& argc, wxChar **argv)
{
    return wxEntryReal(argc, argv);
}

#endif // wxUSE_ON_FATAL_EXCEPTION/!wxUSE_ON_FATAL_EXCEPTION

#endif // wxUSE_BASE

#if wxUSE_GUI

namespace
{

#if wxUSE_UNICODE && !defined(__WXWINCE__)
    #define NEED_UNICODE_CHECK
#endif

#ifdef NEED_UNICODE_CHECK

// check whether Unicode is available
bool wxIsUnicodeAvailable()
{
    static const wchar_t *ERROR_STRING = L"wxWidgets Fatal Error";

    if ( wxGetOsVersion() != wxOS_WINDOWS_NT )
    {
        // we need to be built with MSLU support
#if !wxUSE_UNICODE_MSLU
        // note that we can use MessageBoxW() as it's implemented even under
        // Win9x - OTOH, we can't use wxGetTranslation() because the file APIs
        // used by wxLocale are not
        ::MessageBox
        (
         NULL,
         L"This program uses Unicode and requires Windows NT/2000/XP.\n"
         L"\n"
         L"Program aborted.",
         ERROR_STRING,
         MB_ICONERROR | MB_OK
        );

        return false;
#else // wxUSE_UNICODE_MSLU
        // and the MSLU DLL must also be available
        HMODULE hmod = ::LoadLibraryA("unicows.dll");
        if ( !hmod )
        {
            ::MessageBox
            (
             NULL,
             L"This program uses Unicode and requires unicows.dll to work "
             L"under current operating system.\n"
             L"\n"
             L"Please install unicows.dll and relaunch the program.",
             ERROR_STRING,
             MB_ICONERROR | MB_OK
            );
            return false;
        }

        // this is not really necessary but be tidy
        ::FreeLibrary(hmod);

        // finally do the last check: has unicows.lib initialized correctly?
        hmod = ::LoadLibraryW(L"unicows.dll");
        if ( !hmod )
        {
            ::MessageBox
            (
             NULL,
             L"This program uses Unicode but is not using unicows.dll\n"
             L"correctly and so cannot work under current operating system.\n"
             L"Please contact the program author for an updated version.\n"
             L"\n"
             L"Program aborted.",
             ERROR_STRING,
             MB_ICONERROR | MB_OK
            );

            return false;
        }

        ::FreeLibrary(hmod);
#endif // !wxUSE_UNICODE_MSLU
    }

    return true;
}

#endif // NEED_UNICODE_CHECK

void wxSetProcessDPIAware()
{
#if wxUSE_DYNLIB_CLASS
    typedef BOOL (WINAPI *SetProcessDPIAware_t)(void);
    wxDynamicLibrary dllUser32(wxT("user32.dll"));
    SetProcessDPIAware_t pfnSetProcessDPIAware =
        (SetProcessDPIAware_t)dllUser32.RawGetSymbol(wxT("SetProcessDPIAware"));

    if ( pfnSetProcessDPIAware )
        pfnSetProcessDPIAware();
#endif // wxUSE_DYNLIB_CLASS
}

// It is sometimes undesirable to force DPI awareness on appplications, e.g.
// when they are artwork heavy and don't have appropriately scaled bitmaps, or
// when they are using non-wx, DPI-unaware code. Allow disabling
// SetProcessDPIAware() call.
//
// Further discussion:
//     http://trac.wxwidgets.org/ticket/16116
//     https://groups.google.com/d/topic/wx-dev/Z0VpgzCY34U/discussion
bool gs_allowChangingDPIAwareness = true;

} //anonymous namespace

void wxMSWDisableSettingHighDPIAware()
{
    gs_allowChangingDPIAwareness = false;
}

// ----------------------------------------------------------------------------
// Windows-specific wxEntry
// ----------------------------------------------------------------------------

struct wxMSWCommandLineArguments
{
    wxMSWCommandLineArguments() { argc = 0; argv = NULL; }

    void Init(const wxArrayString& args)
    {
        argc = args.size();

        // +1 here for the terminating NULL
        argv = new wxChar *[argc + 1];
        for ( int i = 0; i < argc; i++ )
        {
            argv[i] = wxStrdup(args[i].t_str());
        }

        // argv[] must be NULL-terminated
        argv[argc] = NULL;
    }

    void Free()
    {
        if ( !argc )
            return;

        for ( int i = 0; i < argc; i++ )
        {
            free(argv[i]);
        }

        wxDELETEA(argv);
        argc = 0;
    }

    int argc;
    wxChar **argv;
};

static wxMSWCommandLineArguments wxArgs;

// common part of wxMSW-specific wxEntryStart() and wxEntry() overloads
static bool
wxMSWEntryCommon(HINSTANCE hInstance, int nCmdShow)
{
    // the first thing to do is to check if we're trying to run an Unicode
    // program under Win9x w/o MSLU emulation layer - if so, abort right now
    // as it has no chance to work and has all chances to crash
#ifdef NEED_UNICODE_CHECK
    if ( !wxIsUnicodeAvailable() )
        return false;
#endif // NEED_UNICODE_CHECK


    // remember the parameters Windows gave us
    wxSetInstance(hInstance);
#ifdef __WXMSW__
    wxApp::m_nCmdShow = nCmdShow;
#endif

    // parse the command line: we can't use pCmdLine in Unicode build so it is
    // simpler to never use it at all (this also results in a more correct
    // argv[0])

    // break the command line in words
    wxArrayString args;

    const wxChar *cmdLine = ::GetCommandLine();
    if ( cmdLine )
    {
        args = wxCmdLineParser::ConvertStringToArgs(cmdLine);
    }

#ifdef __WXWINCE__
    // WinCE doesn't insert the program itself, so do it ourselves.
    args.Insert(wxGetFullModuleName(), 0);
#endif

    wxArgs.Init(args);

    return true;
}

WXDLLEXPORT bool wxEntryStart(HINSTANCE hInstance,
                              HINSTANCE WXUNUSED(hPrevInstance),
                              wxCmdLineArgType WXUNUSED(pCmdLine),
                              int nCmdShow)
{
    if ( !wxMSWEntryCommon(hInstance, nCmdShow) )
       return false;

    return wxEntryStart(wxArgs.argc, wxArgs.argv);
}

WXDLLEXPORT int wxEntry(HINSTANCE hInstance,
                        HINSTANCE WXUNUSED(hPrevInstance),
                        wxCmdLineArgType WXUNUSED(pCmdLine),
                        int nCmdShow)
{
    // wxWidgets library doesn't have problems with non-default DPI settings,
    // so we can mark the app as "DPI aware" for Vista/Win7 (see
    // http://msdn.microsoft.com/en-us/library/dd464659%28VS.85%29.aspx).
    // Note that we intentionally do it here and not in wxApp, so that it
    // doesn't happen if wx code is hosted in another app (e.g. a plugin).
    // It can be disabled by calling wxMSWAllowChangingDPIAwareness().
    if ( gs_allowChangingDPIAwareness )
        wxSetProcessDPIAware();

    if ( !wxMSWEntryCommon(hInstance, nCmdShow) )
        return -1;

    wxON_BLOCK_EXIT_OBJ0(wxArgs, wxMSWCommandLineArguments::Free);

    return wxEntry(wxArgs.argc, wxArgs.argv);
}

#endif // wxUSE_GUI

// ----------------------------------------------------------------------------
// global HINSTANCE
// ----------------------------------------------------------------------------

#if wxUSE_BASE

HINSTANCE wxhInstance = 0;

extern "C" HINSTANCE wxGetInstance()
{
    return wxhInstance;
}

void wxSetInstance(HINSTANCE hInst)
{
    wxhInstance = hInst;
}

#endif // wxUSE_BASE
