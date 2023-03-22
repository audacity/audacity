/**********************************************************************

Audacity: A Digital Audio Editor

TimeTrackVRulerControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/


#include "TimeTrackVRulerControls.h"
#include "TimeTrackVZoomHandle.h"

#include "../../../HitTestResult.h"

#include "AColor.h"
#include "AllThemeResources.h"
#include "ProjectHistory.h"
#include "../../../RefreshCode.h"
#include "Theme.h"
#include "TimeTrack.h"
#include "../../../TrackArtist.h"
#include "../../../TrackPanelDrawingContext.h"
#include "../../../TrackPanelMouseEvent.h"
#include "../../../UIHandle.h"
#include "../../../widgets/Ruler.h"
#include "../../../widgets/LinearUpdater.h"
#include "../../../widgets/LogarithmicUpdater.h"
#include "../../../widgets/RealFormat.h"

TimeTrackVRulerControls::~TimeTrackVRulerControls()
{
}

namespace {
   Ruler &ruler()
   {
      static Ruler theRuler{
         LinearUpdater::Instance(), RealFormat::LinearInstance()
      };
      return theRuler;
   }
}

std::vector<UIHandlePtr> TimeTrackVRulerControls::HitTest(
   const TrackPanelMouseState &st,
   const AudacityProject *pProject)
{
   std::vector<UIHandlePtr> results;

   if ( st.state.GetX() <= st.rect.GetRight() - kGuard ) {
      auto pTrack = FindTrack()->SharedPointer<TimeTrack>(  );
      if (pTrack) {
         auto result = std::make_shared<TimeTrackVZoomHandle>(
            pTrack, st.rect, st.state.m_y );
         result = AssignUIHandlePtr(mVZoomHandle, result);
         results.push_back(result);
      }
   }

   auto more = TrackVRulerControls::HitTest(st, pProject);
   std::copy(more.begin(), more.end(), std::back_inserter(results));

   return results;
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
      if (t && t->vrulerSize.first < rect.GetWidth()) {
         int adj = rr.GetWidth() - t->vrulerSize.first;
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
   vruler->SetFormat(tt->GetDisplayLog()
      ? &RealFormat::LogInstance()
      : &RealFormat::LinearInstance());
   vruler->SetUnits({});
   vruler->SetLabelEdges(false);
   if (tt->GetDisplayLog())
      vruler->SetUpdater(&LogarithmicUpdater::Instance());
   else
      vruler->SetUpdater(&LinearUpdater::Instance());

   vruler->GetMaxSize( &tt->vrulerSize.first, &tt->vrulerSize.second );
}
