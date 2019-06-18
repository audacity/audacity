/**********************************************************************

Audacity: A Digital Audio Editor

LabelTrackView.h

Paul Licameli split from class LabelTrack

**********************************************************************/

#ifndef __AUDACITY_LABEL_TRACK_VIEW__
#define __AUDACITY_LABEL_TRACK_VIEW__

#include "../../ui/CommonTrackView.h"

class LabelTrackView final : public CommonTrackView
{
   LabelTrackView( const LabelTrackView& ) = delete;
   LabelTrackView &operator=( const LabelTrackView& ) = delete;

public:
   explicit
   LabelTrackView( const std::shared_ptr<Track> &pTrack )
      : CommonTrackView{ pTrack } {}
   ~LabelTrackView() override;
};

#endif
