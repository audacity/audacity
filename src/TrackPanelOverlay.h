/**********************************************************************

Audacity: A Digital Audio Editor

TrackPanelOverlay.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_TRACK_PANEL_OVERLAY__
#define __AUDACITY_TRACK_PANEL_OVERLAY__

class TrackPanelCellIterator;

#include <utility>
class wxDC;
class wxRect;
class wxSize;

class TrackPanelOverlay
{
public:
   virtual ~TrackPanelOverlay() = 0;

   // nonvirtual wrapper
   std::pair<wxRect, bool> GetRectangle(wxSize size);

   // size passes the dimensions of the backing dc
   // First member of pair is the rectangle that would be erased
   // Second member of pair indicates whether the overlay is out of date
   virtual std::pair<wxRect, bool> DoGetRectangle(wxSize size) = 0;

   // Default implementation blits from backing store over GetRectangle().first
   virtual void Erase(wxDC &dc, wxDC &src);

   // Draw; dc.GetSize() tells you the total dimensions, and the iterators let you
   // find the rectangles of tracks (or other sub-rectangles of the panel)
   virtual void Draw
      (wxDC &dc, TrackPanelCellIterator begin, TrackPanelCellIterator end) = 0;
};

#endif
