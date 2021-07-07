/**********************************************************************

Audacity: A Digital Audio Editor

TrackPanelMouseEvent.h

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_TRACK_PANEL_MOUSE_EVENT__
#define __AUDACITY_TRACK_PANEL_MOUSE_EVENT__

class wxMouseEvent;
class wxMouseState;
class wxRect;
class wxSize;
class TrackPanelCell;
#include <memory>

// This is a hack so that the code that fakes a MOUSE_LEFT_BTN_UP on
// capture lost doesn't get in the way of handling MOUSE_RIGHT_BTN_UP.
const int kCaptureLostEventId = 19019;

// Augment a mouse state with information about which track panel cell and
// sub-rectangle was hit.
struct TrackPanelMouseState
{
   TrackPanelMouseState
      ( wxMouseState &state_, const wxRect &rect_,
        const std::shared_ptr<TrackPanelCell> &pCell_ )
      : state{ state_ }
      , rect{ rect_ }
      , pCell{ pCell_ }
   {
   }

   wxMouseState &state;
   const wxRect &rect;
   std::shared_ptr<TrackPanelCell> pCell; // may be NULL
};

// Augment a mouse event with information about which track panel cell and
// sub-rectangle was hit.
struct TrackPanelMouseEvent
{
   TrackPanelMouseEvent
      ( wxMouseEvent &event_, const wxRect &rect_, const wxSize &whole_,
        const std::shared_ptr<TrackPanelCell> &pCell_ )
      : event{ event_ }
      , rect{ rect_ }
      , whole{ whole_ }
      , pCell{ pCell_ }
      , steps{ 0 }
      , horizontal{ false }
   {
   }

   wxMouseEvent &event;
   const wxRect &rect;
   const wxSize &whole;
   std::shared_ptr<TrackPanelCell> pCell; // may be NULL
   double steps;  // for mouse wheel rotation
   bool horizontal;  // true for horizontal mouse wheel motion
};

#endif
