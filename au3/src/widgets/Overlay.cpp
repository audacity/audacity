//
//  Overlay.cpp
//  Audacity
//
//  Created by Paul Licameli on 5/7/16.
//
//

#include "Overlay.h"

#include <wx/dc.h>

Overlay::~Overlay()
{
}

std::pair<wxRect, bool> Overlay::GetRectangle(wxSize size)
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

void Overlay::Erase(wxDC& dc, wxDC& src)
{
    wxRect rect(dc.GetSize());
    rect.Intersect(src.GetSize());
    auto smallRect(GetRectangle(src.GetSize()).first);
    rect.Intersect(smallRect);
    if (!rect.IsEmpty()) {
        dc.Blit(rect.x, rect.y, rect.width, rect.height,
                &src, rect.x, rect.y);
    }
}
