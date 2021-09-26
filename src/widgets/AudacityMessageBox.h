/**********************************************************************

  Audacity: A Digital Audio Editor

  AudacityMessageBox.h

  Paul Licameli split this out of ErrorDialog.h

**********************************************************************/

#ifndef __AUDACITY_MESSAGE_BOX__
#define __AUDACITY_MESSAGE_BOX__

#include <wx/msgdlg.h>
#include "Internat.h"

extern AUDACITY_DLL_API TranslatableString AudacityMessageBoxCaptionStr();

// Do not use wxMessageBox!!  Its default window title does not translate!
AUDACITY_DLL_API int AudacityMessageBox(const TranslatableString& message,
   const TranslatableString& caption = XO("Message"),
   long style = wxOK | wxCENTRE,
   wxWindow *parent = NULL,
   int x = wxDefaultCoord, int y = wxDefaultCoord);

#endif
