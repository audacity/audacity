/**********************************************************************

Audacity: A Digital Audio Editor

WaveTrackVZoomHandle.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_WAVE_TRACK_VZOOM_HANDLE__
#define __AUDACITY_WAVE_TRACK_VZOOM_HANDLE__

class wxMouseState;
class WaveTrack;
#include <wx/gdicmn.h>
#include "../../../../MemoryX.h"
#include "../../../../UIHandle.h"


// Note that these can be with or without spectrum view which
// adds a constant.
const int kZoom1to1 = 1;
const int kZoomTimes2 = 2;
const int kZoomDiv2 = 3;
const int kZoomHalfWave = 4;
const int kZoomInByDrag = 5;
const int kZoomIn = 6;
const int kZoomOut = 7;
const int kZoomReset = 8;

class WaveTrackVZoomHandle : public UIHandle
{
   WaveTrackVZoomHandle(const WaveTrackVZoomHandle&);
   static HitTestPreview HitPreview(const wxMouseState &state);

public:
   explicit WaveTrackVZoomHandle
      (const std::shared_ptr<WaveTrack> &pTrack, const wxRect &rect, int y);

   WaveTrackVZoomHandle &operator=(const WaveTrackVZoomHandle&) = default;

   static void DoZoom
   (AudacityProject *pProject,
    WaveTrack *pTrack, bool allChannels, int ZoomKind,
    const wxRect &rect, int zoomStart, int zoomEnd,
    bool fixedMousePoint);

   virtual ~WaveTrackVZoomHandle();

   std::shared_ptr<WaveTrack> GetTrack() const { return mpTrack.lock(); }

   void Enter(bool forward) override;

   Result Click
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override;

   Result Drag
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override;

   HitTestPreview Preview
      (const TrackPanelMouseState &state, const AudacityProject *pProject)
      override;

   Result Release
      (const TrackPanelMouseEvent &event, AudacityProject *pProject,
       wxWindow *pParent) override;

   Result Cancel(AudacityProject *pProject) override;

   void DrawExtras
      (DrawingPass pass,
       wxDC * dc, const wxRegion &updateRegion, const wxRect &panelRect)
      override;

private:
   std::weak_ptr<WaveTrack> mpTrack;

   int mZoomStart{}, mZoomEnd{};
   wxRect mRect{};
};

#endif
