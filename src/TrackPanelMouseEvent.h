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
#include "MemoryX.h"

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
   {
   }

   wxMouseEvent &event;
   const wxRect &rect;
   const wxSize &whole;
   std::shared_ptr<TrackPanelCell> pCell; // may be NULL
   double steps;  // for mouse wheel rotation
};

#endif
