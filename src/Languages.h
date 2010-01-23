/**********************************************************************

  Audacity: A Digital Audio Editor

  Languages.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_LANGUAGES__
#define __AUDACITY_LANGUAGES__

#include <wx/arrstr.h>
#include <wx/string.h>
#include <wx/list.h>

void GetLanguages(wxArrayString &langCodes, wxArrayString &langNames);

wxString GetSystemLanguageCode();

#endif // __AUDACITY_LANGUAGES__

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: ee4ad724-de43-4941-851a-84da60a5c06d

