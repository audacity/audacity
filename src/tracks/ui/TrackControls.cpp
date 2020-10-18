/**********************************************************************

Audacity: A Digital Audio Editor

TrackControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/


#include "TrackControls.h"
#include "AColor.h"
#include "TrackPanelDrawingContext.h"
#include <wx/dc.h>

#include "Track.h"

TrackControls::TrackControls( std::shared_ptr<Track> pTrack )
   : CommonTrackCell{ pTrack }
{
}

TrackControls::~TrackControls()
{
}

static const AttachedTrackObjects::RegisteredFactory key{
   []( Track &track ){
      return DoGetControls::Call( track );
   }
};

std::vector<UIHandlePtr> TrackControls::HitTest(
   const TrackPanelMouseState &state, const AudacityProject *)
{
   return {};
}

void TrackControls::Draw(
   TrackPanelDrawingContext &context, const wxRect &rect, unsigned iPass )
{
   if (iPass == 0) {
      auto &dc = context.dc;
      auto pTrack = FindTrack();
      AColor::MediumTrackInfo(&dc, pTrack && pTrack->GetSelected() );
      dc.DrawRectangle(rect);
   }
}

TrackControls &TrackControls::Get( Track &track )
{
   return track.AttachedObjects::Get< TrackControls >( key );
}

const TrackControls &TrackControls::Get( const Track &track )
{
   return Get( const_cast< Track & >( track ) );
}

DEFINE_ATTACHED_VIRTUAL(DoGetControls) {
   return [](Track &track){
      return std::make_shared<TrackControls>(track.shared_from_this());
   };
}
