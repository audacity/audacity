/**********************************************************************

  Audacity: A Digital Audio Editor

  LangChoice.h

  Dominic Mazzoni

  Pop up a language asking the user to choose a (natural)
  language for the user interface.  Generally only popped
  up once, the first time the program is run.

**********************************************************************/

#ifndef __AUDACITY_LANG_CHOICE__
#define __AUDACITY_LANG_CHOICE__

#include <wx/string.h>

wxString ChooseLanguage(wxWindow *parent);

#endif // __AUDACITY_LANG_CHOICE__

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: b3c8ede6-2b3f-4b16-bc21-f564b7340445

