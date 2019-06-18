/**********************************************************************

Audacity: A Digital Audio Editor

WaveTrackView.h

Paul Licameli split from class WaveTrack

**********************************************************************/

#ifndef __AUDACITY_WAVE_TRACK_VIEW__
#define __AUDACITY_WAVE_TRACK_VIEW__

#include "../../../ui/TrackView.h"

class WaveTrackView final : public CommonTrackView
{
   WaveTrackView( const WaveTrackView& ) = delete;
   WaveTrackView &operator=( const WaveTrackView& ) = delete;

public:
   explicit
   WaveTrackView( const std::shared_ptr<Track> &pTrack )
      : CommonTrackView{ pTrack } {}
   ~WaveTrackView() override;
};

#endif
