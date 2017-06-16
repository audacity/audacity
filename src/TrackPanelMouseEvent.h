/**********************************************************************

Audacity: A Digital Audio Editor

TrackPanelMouseEvent.h

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_TRACK_PANEL_MOUSE_EVENT__
#define __AUDACITY_TRACK_PANEL_MOUSE_EVENT__

class wxMouseEvent;
class wxRect;
class wxSize;
class TrackPanelCell;

// Augment a mouse event with information about which track panel cell and
// sub-rectangle was hit.
struct TrackPanelMouseEvent
{
   TrackPanelMouseEvent
      ( wxMouseEvent &event_, const wxRect &rect_, const wxSize &whole_,
        TrackPanelCell *pCell_ )
      : event{ event_ }
      , rect{ rect_ }
      , whole{ whole_ }
      , pCell{ pCell_ }
      , steps{ 0 }
   {
   }

   wxMouseEvent &event;
   const wxRect &rect;
   const wxSize &whole;
   TrackPanelCell *pCell; // may be NULL
   double steps;  // for mouse wheel rotation
};

#endif
