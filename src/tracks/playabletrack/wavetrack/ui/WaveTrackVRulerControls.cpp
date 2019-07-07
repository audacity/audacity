/**********************************************************************

Audacity: A Digital Audio Editor

WaveTrackVRulerControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../../Audacity.h"
#include "WaveTrackVRulerControls.h"

#include "SpectrumVRulerControls.h"
#include "WaveformVRulerControls.h"
#include "WaveformVZoomHandle.h"

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
WaveTrackVRulerControls::~WaveTrackVRulerControls()
{
}

std::vector<UIHandlePtr> WaveTrackVRulerControls::HitTest
(const TrackPanelMouseState &st,
 const AudacityProject *pProject)
{
   std::vector<UIHandlePtr> results;

   if ( st.state.GetX() <= st.rect.GetRight() - kGuard ) {
      auto pTrack = FindTrack()->SharedPointer<WaveTrack>(  );
      if (pTrack) {
         auto result = std::make_shared<WaveTrackVZoomHandle>(
            pTrack, st.rect, st.state.m_y );
         result = AssignUIHandlePtr(mVZoomHandle, result);
         results.push_back(result);
      }
   }

   auto more = TrackVRulerControls::HitTest(st, pProject);
   std::copy(more.begin(), more.end(), std::back_inserter(results));

   return results;
}

unsigned WaveTrackVRulerControls::HandleWheelRotation
(const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   using namespace RefreshCode;
   const auto pTrack = FindTrack();
   if (!pTrack)
      return RefreshNone;
   const auto wt = static_cast<WaveTrack*>(pTrack.get());
   if (wt->GetDisplay() == WaveTrackViewConstants::Spectrum)
      return SpectrumVRulerControls::DoHandleWheelRotation( evt, pProject, wt );
   else
      return WaveformVRulerControls::DoHandleWheelRotation( evt, pProject, wt );
}

Ruler &WaveTrackVRulerControls::ScratchRuler()
{
   static Ruler theRuler;
   return theRuler;
}

void WaveTrackVRulerControls::Draw(
   TrackPanelDrawingContext &context,
   const wxRect &rect_, unsigned iPass )
{
   TrackVRulerControls::Draw( context, rect_, iPass );
   DoDraw( *this, context, rect_, iPass );
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

void WaveTrackVRulerControls::UpdateRuler( const wxRect &rect )
{
   const auto wt = std::static_pointer_cast< WaveTrack >( FindTrack() );
   if (!wt)
      return;

   const int display = wt->GetDisplay();
   
   if (display == WaveTrackViewConstants::Waveform)
      WaveformVRulerControls::DoUpdateVRuler( rect, wt.get() );
   else
      SpectrumVRulerControls::DoUpdateVRuler( rect, wt.get() );
}
