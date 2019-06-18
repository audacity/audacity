/**********************************************************************

Audacity: A Digital Audio Editor

CommonTrackView.h

Paul Licameli split from class TrackView

**********************************************************************/

#ifndef __AUDACITY_COMMON_TRACK_VIEW__
#define __AUDACITY_COMMON_TRACK_VIEW__

#include "TrackView.h" // to inherit

class CommonTrackView /* not final */ : public TrackView
{
public:
   using TrackView::TrackView;

   std::vector<UIHandlePtr> HitTest
      (const TrackPanelMouseState &, const AudacityProject *pProject)
      final override;

   // Delegates the handling to the related TCP cell
   std::shared_ptr<TrackPanelCell> ContextMenuDelegate() override;

protected:
   Track *GetTrack() const;
};

#endif
