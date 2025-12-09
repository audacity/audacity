/**********************************************************************

  Audacity: A Digital Audio Editor

  Keyboard.cpp

  Dominic Mazzoni
  Brian Gunlogson

**********************************************************************/

#include "Keyboard.h"

#include <wx/event.h>

NormalizedKeyString::NormalizedKeyString(const wxString& key)
    : NormalizedKeyStringBase(key)
{
#if defined(__WXMAC__)
    wxString newkey;
    wxString temp = key;

    // PRL:  This is needed to parse older preference files.
    temp.Replace(wxT("XCtrl+"), wxT("Control+"));

    // PRL:  RawCtrl is the proper replacement for Control, when formatting
    // wxMenuItem, so that wxWidgets shows ^ in the menu.  It is written into
    // NEW preference files (2.2.0 and later).
    temp.Replace(wxT("RawCtrl+"), wxT("Control+"));
    temp.Replace(wxT("Ctrl+"), wxT("Command+"));

    if (temp.Contains(wxT("Control+"))) {
        newkey += wxT("RawCtrl+");
    }

    if (temp.Contains(wxT("Alt+")) || temp.Contains(wxT("Option+"))) {
        newkey += wxT("Alt+");
    }

    if (temp.Contains(wxT("Shift+"))) {
        newkey += wxT("Shift+");
    }

    if (temp.Contains(wxT("Command+"))) {
        newkey += wxT("Ctrl+");
    }

    (NormalizedKeyStringBase&)*this
        =newkey + temp.AfterLast(wxT('+'));
#else
    (NormalizedKeyStringBase&)*this = key;
#endif
}

wxString NormalizedKeyString::Display(bool usesSpecialChars) const
{
    (void)usesSpecialChars;//compiler food
    // using GET to manipulate key string as needed for macOS differences
    // in displaying of it
    auto newkey = this->GET();
#if defined(__WXMAC__)

    if (!usesSpecialChars) {
        // Compose user-visible keystroke names, all ASCII
        newkey.Replace(wxT("RawCtrl+"), wxT("Control+"));
        newkey.Replace(wxT("Alt+"), wxT("Option+"));
        newkey.Replace(wxT("Ctrl+"), wxT("Command+"));
    } else {
        // Compose user-visible keystroke names, with special characters
        newkey.Replace(wxT("Shift+"), wxT("\u21e7"));
        newkey.Replace(wxT("RawCtrl+"), '^');
        newkey.Replace(wxT("Alt+"), wxT("\u2325"));
        newkey.Replace(wxT("Ctrl+"), wxT("\u2318"));
    }

#endif

    return newkey;
}
