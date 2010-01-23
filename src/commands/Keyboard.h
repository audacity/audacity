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
wxString KeyStringDisplay(const wxString & key);
wxString KeyEventToKeyString(const wxKeyEvent & keyEvent);

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: c4be8eb8-6ee6-4831-bedf-072750e88f23

