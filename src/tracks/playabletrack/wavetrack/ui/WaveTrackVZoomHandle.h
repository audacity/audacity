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

struct HitTestResult;

class WaveTrackVZoomHandle : public UIHandle
{
   WaveTrackVZoomHandle();
   WaveTrackVZoomHandle(const WaveTrackVZoomHandle&);
   WaveTrackVZoomHandle &operator=(const WaveTrackVZoomHandle&);
   static WaveTrackVZoomHandle& Instance();
   static HitTestPreview HitPreview(const wxMouseState &state);

public:
   static HitTestResult HitTest(const wxMouseState &state);

   static void DoZoom
   (AudacityProject *pProject,
    WaveTrack *pTrack, bool shiftDown, bool rightUp,
    const wxRect &rect, int zoomStart, int zoomEnd,
    bool fixedMousePoint);

   virtual ~WaveTrackVZoomHandle();

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
