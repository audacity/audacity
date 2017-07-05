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

   HitTestPreview(const wxString &message_, wxCursor *cursor_
      , unsigned refreshCode_ = 0)
      : message(message_), cursor(cursor_), refreshCode(refreshCode_)
   {}

   wxString message {};
   wxCursor *cursor {};

   // Making this non-zero allows mouse-over highlighting
   // See RefreshCode.h for bit flags:
   unsigned refreshCode {};
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
