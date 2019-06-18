/**********************************************************************

Audacity: A Digital Audio Editor

TrackView.h

Paul Licameli split from class Track

**********************************************************************/

#ifndef __AUDACITY_TRACK_VIEW__
#define __AUDACITY_TRACK_VIEW__

#include <memory>
#include "CommonTrackPanelCell.h" // to inherit

class Track;

class TrackView /* not final */ : public CommonTrackCell
{
   TrackView( const TrackView& ) = delete;
   TrackView &operator=( const TrackView& ) = delete;

public:
   explicit
   TrackView( const std::shared_ptr<Track> &pTrack )
      : CommonTrackCell{ pTrack } {}
   virtual ~TrackView() = 0;

   static TrackView &Get( Track & );
   static const TrackView &Get( const Track & );
};

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
