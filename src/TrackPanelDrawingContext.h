/**********************************************************************

 Audacity: A Digital Audio Editor

 TrackPanelDrawingContext.h

 Paul Licameli

 **********************************************************************/

#ifndef __AUDACITY_TRACK_PANEL_DRAWING_CONTEXT__
#define __AUDACITY_TRACK_PANEL_DRAWING_CONTEXT__

#include <memory>
#include "UIHandle.h"

class wxDC;

#include <wx/mousestate.h> // member variable

struct TrackPanelDrawingContext {
   wxDC &dc;
   UIHandlePtr target;
   wxMouseState lastState;

   void *pUserData;

   // This redundancy fixes an MSVC compiler warning:
   TrackPanelDrawingContext() = delete;

   template<typename Handle> auto HighlightedHandle() const
   {
      return Experimental::TrackPanelHighlighting
         ? dynamic_cast<const Handle*>(this->target.get()) : nullptr;
   }

   bool ShouldHighlight(const wxRect &rect) const
   {
      return Experimental::TrackPanelHighlighting &&
         rect.Contains(lastState.GetPosition());
   }
};

#endif
