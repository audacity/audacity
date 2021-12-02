/**********************************************************************

Audacity: A Digital Audio Editor

TrackControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/


#include "TrackControls.h"

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

TrackControls &TrackControls::Get( Track &track )
{
   return track.AttachedObjects::Get< TrackControls >( key );
}

const TrackControls &TrackControls::Get( const Track &track )
{
   return Get( const_cast< Track & >( track ) );
}

DEFINE_ATTACHED_VIRTUAL(DoGetControls) {
   return nullptr;
}
