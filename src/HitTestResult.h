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

   HitTestPreview(const wxString &message_, wxCursor *cursor_)
      : message(message_), cursor(cursor_)
   {}

   wxString message {};
   wxCursor *cursor {};
};

#endif
