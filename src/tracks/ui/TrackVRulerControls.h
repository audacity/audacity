/**********************************************************************

Audacity: A Digital Audio Editor

TrackVRulerControls.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_TRACK_VRULER_CONTROLS__
#define __AUDACITY_TRACK_VRULER_CONTROLS__

#include "CommonTrackPanelCell.h"

class Track;
class TrackView;
class wxDC;

const int kGuard = 5; // 5 pixels to reduce risk of VZooming accidentally

class TrackVRulerControls /* not final */ : public CommonTrackPanelCell
   , public std::enable_shared_from_this< TrackVRulerControls >
{
public:
   explicit
   TrackVRulerControls( const std::shared_ptr<TrackView> &pTrackView );

   virtual ~TrackVRulerControls() = 0;

   static TrackVRulerControls &Get( Track& );
   static const TrackVRulerControls &Get( const Track& );

   static TrackVRulerControls &Get( TrackView& );
   static const TrackVRulerControls &Get( const TrackView& );

   // Define a default hit test method, just for message and cursor
   std::vector<UIHandlePtr> HitTest
      (const TrackPanelMouseState &state,
       const AudacityProject *pProject) override;

   static void DrawZooming
      ( wxDC *dc, const wxRect &cellRect, const wxRect &panelRect,
        int zoomStart, int zoomEnd);

protected:
   std::shared_ptr<Track> DoFindTrack() override;

   // TrackPanelDrawable implementation
   void Draw(
      TrackPanelDrawingContext &context,
      const wxRect &rect, unsigned iPass ) override;

   Track *GetTrack() const;

   std::weak_ptr<TrackView> mwTrackView;
};

#endif
