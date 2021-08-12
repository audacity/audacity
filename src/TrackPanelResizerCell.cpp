/**********************************************************************

Audacity: A Digital Audio Editor

TrackPanelResizeHandle.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/


#include "TrackPanelResizerCell.h"

#include "AColor.h"
#include "Track.h"
#include "TrackArtist.h"
#include "TrackPanelDrawingContext.h"
#include "TrackPanelResizeHandle.h"
#include "TrackPanelMouseEvent.h"
#include "HitTestResult.h"
#include "ViewInfo.h"
#include "widgets/OverlayPanel.h"

#include <wx/dc.h>
#include <wx/mousestate.h>

TrackPanelResizerCell::TrackPanelResizerCell(
   const std::shared_ptr<Track> &pTrack )
   : mwTrack{ pTrack }
{}

std::vector<UIHandlePtr> TrackPanelResizerCell::HitTest
(const TrackPanelMouseState &st, const AudacityProject *pProject)
{
   (void)pProject;// Compiler food
   std::vector<UIHandlePtr> results;
   auto pTrack = FindTrack();
   if (pTrack) {
      auto result = std::make_shared<TrackPanelResizeHandle>(
         pTrack, st.state.m_y );
      result = AssignUIHandlePtr(mResizeHandle, result);
      results.push_back(result);
   }
   return results;
}

std::shared_ptr<Track> TrackPanelResizerCell::DoFindTrack()
{
   return mwTrack.lock();
}

void TrackPanelResizerCell::Draw(
   TrackPanelDrawingContext &context,
   const wxRect &rect, unsigned iPass )
{
   if ( iPass == TrackArtist::PassMargins ) {
      auto pTrack = FindTrack();
      if ( pTrack ) {
         auto dc = &context.dc;
         const bool last =
            pTrack.get() == *TrackList::Channels( pTrack.get() ).rbegin();
         if ( last ) {
            // Fill in separator area below a track
            AColor::TrackPanelBackground( dc, false );
            dc->DrawRectangle( rect );
         }
         else {
            // Area between channels of a group
            // Paint the channel separator over (what would be) the lower border
            // of this channel, down to and including the upper border of the
            // next channel
            
            ADCChanger cleanup{ dc };
            
            // Paint the left part of the background
            const auto artist = TrackArtist::Get( context );
            auto labelw = artist->pZoomInfo->GetLeftOffset() - 1;
            AColor::MediumTrackInfo( dc, pTrack->GetSelected() );
            dc->DrawRectangle(
               rect.GetX(), rect.GetY(), labelw, rect.GetHeight() );
            
            // Stroke the left border
            dc->SetPen(*wxBLACK_PEN);
            {
               const auto left = rect.GetLeft();
               AColor::Line( *dc, left, rect.GetTop(), left, rect.GetBottom() );
            }
            
            AColor::TrackPanelBackground(dc, false);
            
            wxRect rec{ rect };
            rec.width -= labelw - rec.x;
            rec.x = labelw;
            
            dc->DrawRectangle( wxRect( rec ).Inflate( 0, -kBorderThickness ) );
            
            // These lines stroke over what is otherwise "border" of each
            // channel
            dc->SetBrush(*wxTRANSPARENT_BRUSH);
            dc->SetPen(*wxBLACK_PEN);
            const auto left = rec.GetLeft();
            const auto right = rec.GetRight();
            const auto top = rec.GetTop();
            const auto bottom = rec.GetBottom();
            AColor::Line( *dc, left, top,    right, top    );
            AColor::Line( *dc, left, bottom, right, bottom );
         }
      }
   }
}

static const AttachedTrackObjects::RegisteredFactory key{
   []( Track &track ){
      return std::make_shared<TrackPanelResizerCell>(
         track.shared_from_this() );
   }
};

TrackPanelResizerCell &TrackPanelResizerCell::Get( Track &track )
{
   return track.AttachedObjects::Get< TrackPanelResizerCell >( key );
}

const TrackPanelResizerCell &TrackPanelResizerCell::Get( const Track &track )
{
   return Get( const_cast< Track & >( track ) );
}
