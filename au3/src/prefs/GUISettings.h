/**********************************************************************

Audacity: A Digital Audio Editor

@file GUISettings.h

Paul Licameli split from GUIPrefs.h

**********************************************************************/
#ifndef __AUDACITY_GUI_SETTINGS__
#define __AUDACITY_GUI_SETTINGS__

// Right to left languages fail in many wx3 dialogs with missing buttons.
// The workaround is to use LTR in those dialogs.
#ifndef __WXMAC__
#define RTL_WORKAROUND(pWnd) \
    if (gPrefs->Read("/GUI/RtlWorkaround", true)) \
    pWnd->SetLayoutDirection(wxLayout_LeftToRight);
#else
   #define RTL_WORKAROUND(pWnd)
#endif

class wxString;

namespace GUISettings {
// If no input language given, defaults to system language.
// Returns the language actually used which is not lang if lang cannot be found.
AUDACITY_DLL_API wxString SetLang(const wxString& lang);
}

#endif
