/**********************************************************************

Audacity: A Digital Audio Editor

TimeTrackView.h

Paul Licameli split from class TimeTrack

**********************************************************************/

#ifndef __AUDACITY_TIME_TRACK_VIEW__
#define __AUDACITY_TIME_TRACK_VIEW__

#include "../../ui/TrackView.h"

class TimeTrackView final : public TrackView
{
   TimeTrackView( const TimeTrackView& ) = delete;
   TimeTrackView &operator=( const TimeTrackView& ) = delete;

public:
   explicit
   TimeTrackView( const std::shared_ptr<Track> &pTrack )
      : TrackView{ pTrack } {}
   ~TimeTrackView() override;
};

#endif
