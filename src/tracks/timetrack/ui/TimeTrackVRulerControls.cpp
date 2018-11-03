/**********************************************************************

Audacity: A Digital Audio Editor

TimeTrackVRulerControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../Audacity.h"
#include "TimeTrackVRulerControls.h"

#include "../../../HitTestResult.h"

#include "../../../AColor.h"
#include "../../../AllThemeResources.h"
#include "../../../TimeTrack.h"
#include "../../../TrackArtist.h"
#include "../../../TrackPanelDrawingContext.h"
#include "../../../widgets/Ruler.h"

TimeTrackVRulerControls::~TimeTrackVRulerControls()
{
}

namespace {
   Ruler &ruler()
   {
      static Ruler theRuler;
      return theRuler;
   }
}

void TimeTrackVRulerControls::Draw(
   TrackPanelDrawingContext &context,
   const wxRect &rect_, unsigned iPass )
{
   TrackVRulerControls::Draw( context, rect_, iPass );

   // Draw on a later pass because the bevel overpaints one pixel
   // out of bounds on the bottom

   if ( iPass == TrackArtist::PassControls ) {
      auto t = FindTrack();
      if ( !t )
         return;

      auto rect = rect_;
      --rect.width;
      --rect.height;

      auto dc = &context.dc;
      wxRect bev = rect;
      bev.Inflate(-1, 0);
      bev.width += 1;
      AColor::BevelTrackInfo(*dc, true, bev);
      
      // Right align the ruler
      wxRect rr = rect;
      rr.width--;
      if (t && t->vrulerSize.GetWidth() < rect.GetWidth()) {
         int adj = rr.GetWidth() - t->vrulerSize.GetWidth();
         rr.x += adj;
         rr.width -= adj;
      }
      
      UpdateRuler(rr);

      auto vruler = &ruler();

      vruler->SetTickColour( theTheme.Colour( clrTrackPanelText ));
      vruler->Draw(*dc);
   }
}

void TimeTrackVRulerControls::UpdateRuler( const wxRect &rect )
{
   const auto tt = std::static_pointer_cast< TimeTrack >( FindTrack() );
   if (!tt)
      return;
   auto vruler = &ruler();

   float min, max;
   min = tt->GetRangeLower() * 100.0;
   max = tt->GetRangeUpper() * 100.0;

   vruler->SetDbMirrorValue( 0.0 );
   vruler->SetBounds(rect.x, rect.y, rect.x + rect.width, rect.y + rect.height-1);
   vruler->SetOrientation(wxVERTICAL);
   vruler->SetRange(max, min);
   vruler->SetFormat((tt->GetDisplayLog()) ? Ruler::RealLogFormat : Ruler::RealFormat);
   vruler->SetUnits(wxT(""));
   vruler->SetLabelEdges(false);
   vruler->SetLog(tt->GetDisplayLog());

   vruler->GetMaxSize( &tt->vrulerSize.x, &tt->vrulerSize.y );
}
