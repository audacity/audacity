/**********************************************************************

Audacity: A Digital Audio Editor

TrackControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../Audacity.h"
#include "TrackControls.h"

TrackControls::TrackControls( std::shared_ptr<Track> pTrack )
   : CommonTrackCell{ pTrack }
{
}

TrackControls::~TrackControls()
{
}
