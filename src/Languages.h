/**********************************************************************

  Audacity: A Digital Audio Editor

  Languages.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_LANGUAGES__
#define __AUDACITY_LANGUAGES__

class wxArrayString;
class wxString;

#include "audacity/Types.h"

void GetLanguages(
   wxArrayString &langCodes, TranslatableStrings &langNames);

wxString GetSystemLanguageCode();

#endif // __AUDACITY_LANGUAGES__
