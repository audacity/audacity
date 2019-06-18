/**********************************************************************

Audacity: A Digital Audio Editor

TrackControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../Audacity.h"
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
   return *static_cast<TrackControls*>( track.GetTrackControls().get() );
}

const TrackControls &TrackControls::Get( const Track &track )
{
   return *static_cast<const TrackControls*>( track.GetTrackControls().get() );
}

