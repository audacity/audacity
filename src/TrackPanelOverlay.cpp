/**********************************************************************

Audacity: A Digital Audio Editor

TrackPanelOverlay.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "TrackPanelOverlay.h"

#include <wx/dc.h>

TrackPanelOverlay::~TrackPanelOverlay()
{
}

std::pair<wxRect, bool> TrackPanelOverlay::GetRectangle(wxSize size)
{
   auto result = DoGetRectangle(size);
#ifdef __WXMAC__
   // On OSX, if a HiDPI resolution is being used, a vertical line will actually take up
   // more than 1 pixel (even though it is drawn as 1), so we restore the surrounding
   // pixels as well.  (This is because the wxClientDC doesn't know about the scaling.
   result.first.Inflate(1, 0);
#endif
   return result;
}

void TrackPanelOverlay::Erase(wxDC &dc, wxDC &src)
{
   wxRect rect(dc.GetSize());
   rect.Intersect(src.GetSize());
   auto smallRect(GetRectangle(src.GetSize()).first);
   rect.Intersect(smallRect);
   if (!rect.IsEmpty())
      dc.Blit(rect.x, rect.y, rect.width, rect.height,
              &src, rect.x, rect.y);
}
