/**********************************************************************

 Audacity: A Digital Audio Editor

 TrackPanelResizerCell.h

 Paul Licameli split from TrackPanel.cpp

 **********************************************************************/

#ifndef __AUDACITY_TRACK_PANEL_RESIZER_CELL__
#define __AUDACITY_TRACK_PANEL_RESIZER_CELL__

#include "tracks/ui/CommonTrackPanelCell.h"

class TrackPanelResizeHandle;

class TrackPanelResizerCell : public CommonTrackPanelCell
{
   TrackPanelResizerCell(const TrackPanelResizerCell&) = delete;
   TrackPanelResizerCell &operator= (const TrackPanelResizerCell&) = delete;
public:

   explicit
   TrackPanelResizerCell( std::shared_ptr<Track> pTrack );

   std::vector<UIHandlePtr> HitTest
      (const TrackPanelMouseState &, const AudacityProject *) override;

protected:
   std::shared_ptr<Track> DoFindTrack() override
   { return mpTrack.lock(); };

private:
   std::weak_ptr<Track> mpTrack;

   std::weak_ptr<TrackPanelResizeHandle> mResizeHandle;
};

#endif
