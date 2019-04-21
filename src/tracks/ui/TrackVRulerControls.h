/**********************************************************************

Audacity: A Digital Audio Editor

TrackVRulerControls.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_TRACK_VRULER_CONTROLS__
#define __AUDACITY_TRACK_VRULER_CONTROLS__

#include "CommonTrackPanelCell.h"
#include "../../MemoryX.h"

class Track;
class wxDC;

const int kGuard = 5; // 5 pixels to reduce risk of VZooming accidentally

class TrackVRulerControls /* not final */ : public CommonTrackPanelCell
{
public:
   explicit
   TrackVRulerControls( std::shared_ptr<Track> pTrack );

   virtual ~TrackVRulerControls() = 0;

   // Define a default hit test method, just for message and cursor
   std::vector<UIHandlePtr> HitTest
      (const TrackPanelMouseState &state,
       const AudacityProject *pProject) override;

   static void DrawZooming
      ( wxDC *dc, const wxRect &cellRect, const wxRect &panelRect,
        int zoomStart, int zoomEnd);

protected:
   std::shared_ptr<Track> DoFindTrack() override;

   Track *GetTrack() const;

   std::weak_ptr<Track> mwTrack;
};

#endif
