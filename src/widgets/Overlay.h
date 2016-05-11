//
//  Overlay.h
//  Audacity
//
//  Created by Paul Licameli on 5/7/16.
//
//

#ifndef __AUDACITY_OVERLAY__
#define __AUDACITY_OVERLAY__

#include <utility>

class OverlayPanel;
class wxDC;
class wxRect;
class wxSize;

class Overlay
{
public:
   virtual ~Overlay() = 0;

   // nonvirtual wrapper
   std::pair<wxRect, bool> GetRectangle(wxSize size);

   // size passes the dimensions of the backing dc
   // First member of pair is the rectangle that would be erased
   // Second member of pair indicates whether the overlay is out of date
   virtual std::pair<wxRect, bool> DoGetRectangle(wxSize size) = 0;

   // Default implementation blits from backing store over GetRectangle().first
   virtual void Erase(wxDC &dc, wxDC &src);

   // Draw; dc.GetSize() tells you the total dimensions, and the panel is supplied
   // as context
   virtual void Draw(OverlayPanel &panel, wxDC &dc) = 0;
};

#endif
