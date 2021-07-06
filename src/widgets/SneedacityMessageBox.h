/**********************************************************************

  Sneedacity: A Digital Audio Editor

  SneedacityMessageBox.h

  Paul Licameli split this out of ErrorDialog.h

**********************************************************************/

#ifndef __SNEEDACITY_MESSAGE_BOX__
#define __SNEEDACITY_MESSAGE_BOX__

#include <wx/msgdlg.h>
#include "Internat.h"

extern SNEEDACITY_DLL_API TranslatableString SneedacityMessageBoxCaptionStr();

// Do not use wxMessageBox!!  Its default window title does not translate!
inline int SneedacityMessageBox(const TranslatableString& message,
   const TranslatableString& caption = SneedacityMessageBoxCaptionStr(),
   long style = wxOK | wxCENTRE,
   wxWindow *parent = NULL,
   int x = wxDefaultCoord, int y = wxDefaultCoord)
{
   return ::wxMessageBox(message.Translation(), caption.Translation(),
      style, parent, x, y);
}

#endif
