/**********************************************************************

Audacity: A Digital Audio Editor

LabelTrackView.h

Paul Licameli split from class LabelTrack

**********************************************************************/

#ifndef __AUDACITY_LABEL_TRACK_VIEW__
#define __AUDACITY_LABEL_TRACK_VIEW__

#include "../../ui/TrackView.h"

class LabelTrackView final : public TrackView
{
   LabelTrackView( const LabelTrackView& ) = delete;
   LabelTrackView &operator=( const LabelTrackView& ) = delete;

public:
   explicit
   LabelTrackView( const std::shared_ptr<Track> &pTrack )
      : TrackView{ pTrack } {}
   ~LabelTrackView() override;
};

#endif
