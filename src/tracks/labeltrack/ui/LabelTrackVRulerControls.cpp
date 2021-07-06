/**********************************************************************

Audacity: A Digital Audio Editor

LabelTrackVRulerControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/


#include "LabelTrackVRulerControls.h"

#include "../../../HitTestResult.h"

#include "../../../AColor.h"
#include "../../../TrackArtist.h"
#include "../../../TrackPanelDrawingContext.h"

LabelTrackVRulerControls::~LabelTrackVRulerControls()
{
}

void LabelTrackVRulerControls::Draw(
   TrackPanelDrawingContext &context,
   const wxRect &rect_, unsigned iPass )
{
   TrackVRulerControls::Draw( context, rect_, iPass );

   // Draw on a later pass because the bevel overpaints one pixel
   // out of bounds on the bottom

   if ( iPass == TrackArtist::PassControls ) {
      auto rect = rect_;
      --rect.width;
      --rect.height;

      auto dc = &context.dc;
      wxRect bev = rect;
      bev.Inflate(-1, 0);
      bev.width += 1;
      AColor::BevelTrackInfo(*dc, true, bev);
   }
}

void LabelTrackVRulerControls::UpdateRuler( const wxRect &rect )
{
   // Label tracks do not have a vruler
   // do nothing
}
