/**********************************************************************

Audacity: A Digital Audio Editor

TrackControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/


#include "TrackControls.h"

#include "../../Track.h"

TrackControls::TrackControls( std::shared_ptr<Track> pTrack )
   : CommonTrackCell{ pTrack }
{
}

TrackControls::~TrackControls()
{
}

TrackControls &TrackControls::Get( Track &track )
{
   auto pControls =
      std::static_pointer_cast<TrackControls>( track.GetTrackControls() );
   if (!pControls)
      // create on demand
      track.SetTrackControls( pControls = DoGetControls::Call( track ) );
   return *pControls;
}

const TrackControls &TrackControls::Get( const Track &track )
{
   return Get( const_cast< Track& >( track ) );
}

DEFINE_ATTACHED_VIRTUAL(DoGetControls) {
   return nullptr;
}
