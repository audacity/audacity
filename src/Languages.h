/**********************************************************************

  Audacity: A Digital Audio Editor

  Languages.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_LANGUAGES__
#define __AUDACITY_LANGUAGES__

class wxArrayString;

#include <wx/list.h>

void GetLanguages(wxArrayString &langCodes, wxArrayString &langNames);

wxString GetSystemLanguageCode();

#endif // __AUDACITY_LANGUAGES__
