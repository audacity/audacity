/**********************************************************************

Audacity: A Digital Audio Editor

TimeTrackView.h

Paul Licameli split from class TimeTrack

**********************************************************************/

#ifndef __AUDACITY_TIME_TRACK_VIEW__
#define __AUDACITY_TIME_TRACK_VIEW__

#include "../../ui/CommonTrackView.h"

class TimeTrackView final : public CommonTrackView
{
   TimeTrackView( const TimeTrackView& ) = delete;
   TimeTrackView &operator=( const TimeTrackView& ) = delete;

public:
   explicit
   TimeTrackView( const std::shared_ptr<Track> &pTrack )
      : CommonTrackView{ pTrack } {}
   ~TimeTrackView() override;
};

#endif
