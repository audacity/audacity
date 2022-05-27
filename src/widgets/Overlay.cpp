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

std::pair<wxRect, bool> Overlay::GetRectangle(graphics::Painter &painter, wxSize size)
{
   auto result = DoGetRectangle(painter, size);
#ifdef __WXMAC__
   // On OSX, if a HiDPI resolution is being used, a vertical line will actually take up
   // more than 1 pixel (even though it is drawn as 1), so we restore the surrounding
   // pixels as well.  (This is because the wxClientDC doesn't know about the scaling.
   result.first.Inflate(1, 0);
#endif
   return result;
}
