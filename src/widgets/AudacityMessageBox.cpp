/**********************************************************************

  Audacity: A Digital Audio Editor

  AudacityMessageBox.cpp

  Paul Licameli split this out of ErrorDialog.cpp

**********************************************************************/

#include "AudacityMessageBox.h"
#include "Internat.h"

#include "Journal.h"
#include "wxArrayStringEx.h"

int AudacityMessageBox(const TranslatableString& message,
   const TranslatableString& caption,
   long style, wxWindow *parent, int x, int y)
{
   // wxMessageBox is implemented with native message boxes and does not
   // use the wxWidgets message machinery.  Therefore the wxEventFilter that
   // most journal recording relies on fails us here.  So if replaying, don't
   // really make the modal dialog, but just return the expected value.
   return Journal::IfNotPlaying( L"MessageBox", [&]{
      return ::wxMessageBox(
         message.Translation(), caption.Translation(),
         style, parent, x, y);
   } );
}
