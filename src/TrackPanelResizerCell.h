/**********************************************************************

 Audacity: A Digital Audio Editor

 TrackPanelResizerCell.h

 Paul Licameli split from TrackPanel.cpp

 **********************************************************************/

#ifndef __AUDACITY_TRACK_PANEL_RESIZER_CELL__
#define __AUDACITY_TRACK_PANEL_RESIZER_CELL__

#include "tracks/ui/CommonTrackPanelCell.h"

class TrackPanelResizeHandle;
class TrackView;

class TrackPanelResizerCell : public CommonTrackPanelCell
{
   TrackPanelResizerCell(const TrackPanelResizerCell&) = delete;
   TrackPanelResizerCell &operator= (const TrackPanelResizerCell&) = delete;
public:

   explicit
   TrackPanelResizerCell( const std::shared_ptr<TrackView> &pView );

   std::vector<UIHandlePtr> HitTest
      (const TrackPanelMouseState &, const AudacityProject *) override;

protected:
   std::shared_ptr<Track> DoFindTrack() override;

private:
   // back-pointer is weak to break a cycle
   std::weak_ptr<TrackView> mwView;

   // TrackPanelDrawable implementation
   void Draw(
      TrackPanelDrawingContext &context,
      const wxRect &rect, unsigned iPass ) override;

   std::weak_ptr<TrackPanelResizeHandle> mResizeHandle;
};

#endif
