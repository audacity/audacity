/**********************************************************************

  Audacity: A Digital Audio Editor

  Keyboard.h

  Dominic Mazzoni
  Brian Gunlogson

**********************************************************************/

#include <wx/defs.h>
#include <wx/event.h>
#include <wx/string.h>

wxString KeyStringNormalize(const wxString & key);
wxString KeyStringDisplay(const wxString & key, bool useSpecialChars = false);
wxString KeyEventToKeyString(const wxKeyEvent & keyEvent);
