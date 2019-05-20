/**********************************************************************

  Audacity: A Digital Audio Editor

  AudacityMessageBox.h

  Paul Licameli split this out of ErrorDialog.h

**********************************************************************/

#ifndef __AUDACITY_MESSAGE_BOX__
#define __AUDACITY_MESSAGE_BOX__

#include <wx/msgdlg.h>

extern wxString AudacityMessageBoxCaptionStr();

// Do not use wxMessageBox!!  Its default window title does not translate!
inline int AudacityMessageBox(const wxString& message,
                              const wxString& caption = AudacityMessageBoxCaptionStr(),
                              long style = wxOK | wxCENTRE,
                              wxWindow *parent = NULL,
                              int x = wxDefaultCoord, int y = wxDefaultCoord)
{
   return ::wxMessageBox(message, caption, style, parent, x, y);
}

#endif
