/**********************************************************************

Audacity: A Digital Audio Editor

WaveTrackVRulerControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../../Audacity.h"
#include "WaveTrackVRulerControls.h"

#include "../../../../Experimental.h"

#include "../../../../RefreshCode.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "../../../../WaveTrack.h"

#include "../../../../AColor.h"
#include "../../../../AllThemeResources.h"
#include "../../../../TrackArtist.h"
#include "../../../../TrackPanelDrawingContext.h"
#include "../../../../widgets/Ruler.h"

///////////////////////////////////////////////////////////////////////////////
Ruler &WaveTrackVRulerControls::ScratchRuler()
{
   static Ruler theRuler;
   return theRuler;
}

void WaveTrackVRulerControls::DoDraw( TrackVRulerControls &controls,
   TrackPanelDrawingContext &context,
   const wxRect &rect_, unsigned iPass )
{
   Ruler &vruler = ScratchRuler();

   // Draw on a later pass because the bevel overpaints one pixel
   // out of bounds on the bottom

   if ( iPass == TrackArtist::PassControls ) {
      auto rect = rect_;
      --rect.width;
      --rect.height;

      auto dc = &context.dc;

      // All waves have a ruler in the info panel
      // The ruler needs a bevelled surround.
      wxRect bev = rect;
      bev.Inflate(-1, 0);
      bev.width += 1;

      bool highlight = false;
#ifdef EXPERIMENTAL_TRACK_PANEL_HIGHLIGHTING
      highlight = rect.Contains(context.lastState.GetPosition());
#endif
   
      AColor::BevelTrackInfo(*dc, true, bev, highlight);
      
      // Right align the ruler
      wxRect rr = rect;
      rr.width--;
      
      auto t = controls.FindTrack();
      if ( !t )
         return;

      if ( t->vrulerSize.GetWidth() < rect.GetWidth()) {
         int adj = rr.GetWidth() - t->vrulerSize.GetWidth();
         rr.x += adj;
         rr.width -= adj;
      }
      
      controls.UpdateRuler(rr);
      
      vruler.SetTickColour( theTheme.Colour( clrTrackPanelText ));
      vruler.Draw(*dc);
   }
}
