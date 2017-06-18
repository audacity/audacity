/**********************************************************************

Audacity: A Digital Audio Editor

HitTestResult.h

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_HIT_TEST_RESULT__
#define __AUDACITY_HIT_TEST_RESULT__

#include <wx/string.h>
#include "MemoryX.h"

class UIHandle;
class wxCursor;

using UIHandlePtr = std::shared_ptr<UIHandle>;

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

struct HitTestResult
{
   HitTestResult()
   {}

   HitTestResult(HitTestPreview preview_, UIHandlePtr handle_)
      : preview(preview_), handle(handle_)
   {}

   HitTestPreview preview {};
   UIHandlePtr handle {};
};

#endif
