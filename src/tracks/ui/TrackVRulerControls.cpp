/**********************************************************************

Audacity: A Digital Audio Editor

TrackVRulerControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../Audacity.h"
#include "TrackVRulerControls.h"

#include "TrackView.h"

#include "../../AColor.h"
#include "../../Track.h"
#include "../../TrackArtist.h"
#include "../../TrackPanelDrawingContext.h"
#include "../../ViewInfo.h"

#include <wx/cursor.h>
#include <wx/dc.h>
#include <wx/translation.h>

TrackVRulerControls::TrackVRulerControls(
   const std::shared_ptr<TrackView> &pTrackView )
  : mwTrackView{ pTrackView }
{
}

TrackVRulerControls::~TrackVRulerControls()
{
}

TrackVRulerControls &TrackVRulerControls::Get( Track &track )
{
   return *TrackView::Get( track ).GetVRulerControls();
}

const TrackVRulerControls &TrackVRulerControls::Get( const Track &track )
{
   return *TrackView::Get( track ).GetVRulerControls();
}

TrackVRulerControls &TrackVRulerControls::Get( TrackView &trackView )
{
   return *trackView.GetVRulerControls();
}

const TrackVRulerControls &TrackVRulerControls::Get( const TrackView &trackView )
{
   return *trackView.GetVRulerControls();
}

std::shared_ptr<Track> TrackVRulerControls::DoFindTrack()
{
   const auto pView = mwTrackView.lock();
   if ( pView )
      return pView->FindTrack();
   return {};
}

std::vector<UIHandlePtr> TrackVRulerControls::HitTest
(const TrackPanelMouseState &, const AudacityProject *)
{
   return std::vector<UIHandlePtr>{};
}

void TrackVRulerControls::DrawZooming
   ( wxDC *dc, const wxRect &cellRect, const wxRect &panelRect,
     int zoomStart, int zoomEnd )
{
   // Draw a dashed rectangle, its right side disappearing in the black right
   // border of the track area, which is not part of this cell but right of it.
   wxRect rect;

   dc->SetBrush(*wxTRANSPARENT_BRUSH);
   dc->SetPen(*wxBLACK_DASHED_PEN);

   rect.y = std::min( zoomStart, zoomEnd);
   rect.height = 1 + abs( zoomEnd - zoomStart);

   rect.x = cellRect.x;
   // TODO: Don't use the constant kRightMargin, but somehow discover the
   // neighboring track rectangle
   rect.SetRight(panelRect.GetWidth() - kRightMargin);

   dc->DrawRectangle(rect);
}

void TrackVRulerControls::Draw(
   TrackPanelDrawingContext &context,
   const wxRect &rect_, unsigned iPass )
{
   // Common initial part of drawing for all subtypes
   if ( iPass == TrackArtist::PassMargins ) {
      auto rect = rect_;
      --rect.width;
      
      auto dc = &context.dc;
      
      
      // Paint the background
      auto pTrack = FindTrack();
      AColor::MediumTrackInfo(dc, pTrack && pTrack->GetSelected() );
      dc->DrawRectangle( rect );
      
      // Stroke the left border
      dc->SetPen(*wxBLACK_PEN);
      {
         const auto left = rect.GetLeft();
         AColor::Line( *dc, left, rect.GetTop(), left, rect.GetBottom() );
      }
   }
}
