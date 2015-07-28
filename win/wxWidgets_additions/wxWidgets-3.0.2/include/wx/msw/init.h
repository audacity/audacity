/////////////////////////////////////////////////////////////////////////////
// Name:        wx/msw/init.h
// Purpose:     Windows-specific wxEntry() overload
// Author:      Julian Smart
// Modified by:
// Created:     01/02/97
// Copyright:   (c) Julian Smart
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

#ifndef _WX_MSW_INIT_H_
#define _WX_MSW_INIT_H_

// ----------------------------------------------------------------------------
// Windows-specific wxEntry() overload and wxIMPLEMENT_WXWIN_MAIN definition
// ----------------------------------------------------------------------------

// we need HINSTANCE declaration to define WinMain()
#include "wx/msw/wrapwin.h"

#ifndef SW_SHOWNORMAL
    #define SW_SHOWNORMAL 1
#endif

// WinMain() is always ANSI, even in Unicode build, under normal Windows
// but is always Unicode under CE
#ifdef __WXWINCE__
    typedef wchar_t *wxCmdLineArgType;
#else
    typedef char *wxCmdLineArgType;
#endif

// Call this function to prevent wxMSW from calling SetProcessDPIAware().
// Must be called before wxEntry().
extern WXDLLIMPEXP_CORE void wxMSWDisableSettingHighDPIAware();

// Windows-only overloads of wxEntry() and wxEntryStart() which take the
// parameters passed to WinMain() instead of those passed to main()
extern WXDLLIMPEXP_CORE bool
    wxEntryStart(HINSTANCE hInstance,
                HINSTANCE hPrevInstance = NULL,
                wxCmdLineArgType pCmdLine = NULL,
                int nCmdShow = SW_SHOWNORMAL);

extern WXDLLIMPEXP_CORE int
    wxEntry(HINSTANCE hInstance,
            HINSTANCE hPrevInstance = NULL,
            wxCmdLineArgType pCmdLine = NULL,
            int nCmdShow = SW_SHOWNORMAL);

#if defined(__BORLANDC__) && wxUSE_UNICODE
    // Borland C++ has the following nonstandard behaviour: when the -WU
    // command line flag is used, the linker expects to find wWinMain instead
    // of WinMain. This flag causes the compiler to define _UNICODE and
    // UNICODE symbols and there's no way to detect its use, so we have to
    // define both WinMain and wWinMain so that wxIMPLEMENT_WXWIN_MAIN works
    // for both code compiled with and without -WU.
    // See http://sourceforge.net/tracker/?func=detail&atid=309863&aid=1935997&group_id=9863
    // for more details.
    #define wxIMPLEMENT_WXWIN_MAIN_BORLAND_NONSTANDARD                      \
        extern "C" int WINAPI wWinMain(HINSTANCE hInstance,                 \
                                      HINSTANCE hPrevInstance,              \
                                      wchar_t * WXUNUSED(lpCmdLine),        \
                                      int nCmdShow)                         \
        {                                                                   \
            wxDISABLE_DEBUG_SUPPORT();                                      \
                                                                            \
            /* NB: wxEntry expects lpCmdLine argument to be char*, not */   \
            /*     wchar_t*, but fortunately it's not used anywhere    */   \
            /*     and we can simply pass NULL in:                     */   \
            return wxEntry(hInstance, hPrevInstance, NULL, nCmdShow);       \
        }
#else
    #define wxIMPLEMENT_WXWIN_MAIN_BORLAND_NONSTANDARD
#endif // defined(__BORLANDC__) && wxUSE_UNICODE

#define wxIMPLEMENT_WXWIN_MAIN                                              \
    extern "C" int WINAPI WinMain(HINSTANCE hInstance,                      \
                                  HINSTANCE hPrevInstance,                  \
                                  wxCmdLineArgType WXUNUSED(lpCmdLine),     \
                                  int nCmdShow)                             \
    {                                                                       \
        wxDISABLE_DEBUG_SUPPORT();                                          \
                                                                            \
        /* NB: We pass NULL in place of lpCmdLine to behave the same as  */ \
        /*     Borland-specific wWinMain() above. If it becomes needed   */ \
        /*     to pass lpCmdLine to wxEntry() here, you'll have to fix   */ \
        /*     wWinMain() above too.                                     */ \
        return wxEntry(hInstance, hPrevInstance, NULL, nCmdShow);           \
    }                                                                       \
    wxIMPLEMENT_WXWIN_MAIN_BORLAND_NONSTANDARD


#endif // _WX_MSW_INIT_H_
