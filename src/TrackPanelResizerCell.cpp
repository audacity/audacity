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

#include "graphics/Painter.h"
#include "graphics/WXPainterUtils.h"

TrackPanelResizerCell::TrackPanelResizerCell(
   const std::shared_ptr<Track> &pTrack )
   : CommonTrackCell{ pTrack }
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

void TrackPanelResizerCell::Draw(
   TrackPanelDrawingContext &context,
   const wxRect &rect, unsigned iPass )
{
   if ( iPass == TrackArtist::PassMargins ) {
      auto pTrack = FindTrack();
      if ( pTrack ) {
         auto& painter = context.painter;
         auto stateMutator = painter.GetStateMutator();

         const bool last =
            pTrack.get() == *TrackList::Channels( pTrack.get() ).rbegin();
         if ( last ) {
            // Fill in separator area below a track
            AColor::TrackPanelBackground( stateMutator, false );
            painter.DrawRect( RectFromWXRect( rect ) );
         }
         else {
            // Area between channels of a group
            // Paint the channel separator over (what would be) the lower border
            // of this channel, down to and including the upper border of the
            // next channel
            
            // Paint the left part of the background
            const auto artist = TrackArtist::Get( context );
            auto labelw = artist->pZoomInfo->GetLeftOffset() - 1;
            AColor::MediumTrackInfo( stateMutator, pTrack->GetSelected() );
            painter.DrawRect(
               rect.GetX(), rect.GetY(), labelw, rect.GetHeight() );
            
            // Stroke the left border
            stateMutator.SetPen(Colors::Black);
            {
               const auto left = rect.GetLeft();
               AColor::Line( painter, left, rect.GetTop(), left, rect.GetBottom() );
            }
            
            AColor::TrackPanelBackground(stateMutator, false);
            
            wxRect rec{ rect };
            rec.width -= labelw - rec.x;
            rec.x = labelw;
            
            painter.DrawRect( RectFromWXRect( wxRect( rec ).Inflate( 0, -kBorderThickness ) ) );
            
            // These lines stroke over what is otherwise "border" of each
            // channel
            stateMutator.SetBrush(Brush::NoBrush);
            stateMutator.SetPen(Colors::Black);

            const auto left = rec.GetLeft();
            const auto right = rec.GetRight();
            const auto top = rec.GetTop();
            const auto bottom = rec.GetBottom();
            AColor::Line( painter, left, top,    right, top    );
            AColor::Line( painter, left, bottom, right, bottom );
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
