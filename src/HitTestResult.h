/**********************************************************************

Audacity: A Digital Audio Editor

HitTestResult.h

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_HIT_TEST_RESULT__
#define __AUDACITY_HIT_TEST_RESULT__

#include <wx/string.h>
#include "MemoryX.h"

class wxCursor;

struct HitTestPreview
{
   HitTestPreview()
   {}

   HitTestPreview(const wxString &message_, wxCursor *cursor_,
      const wxString &tooltip_ = wxString{})
      : message(message_), cursor(cursor_), tooltip(tooltip_)
   {}

   wxString message {};
   wxCursor *cursor {};
   wxString tooltip{};
};

#endif
