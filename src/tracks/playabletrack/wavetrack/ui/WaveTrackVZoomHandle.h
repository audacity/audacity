/**********************************************************************

Audacity: A Digital Audio Editor

WaveTrackVZoomHandle.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_WAVE_TRACK_VZOOM_HANDLE__
#define __AUDACITY_WAVE_TRACK_VZOOM_HANDLE__

class wxMouseState;
class WaveTrack;
#include "WaveTrackViewConstants.h"
#include "../../../../UIHandle.h"


class WaveTrackVZoomHandle : public UIHandle
{
   WaveTrackVZoomHandle(const WaveTrackVZoomHandle&);
   static HitTestPreview HitPreview(const wxMouseState &state);

public:
   explicit WaveTrackVZoomHandle
      (const std::shared_ptr<WaveTrack> &pTrack, const wxRect &rect, int y);

   WaveTrackVZoomHandle &operator=(const WaveTrackVZoomHandle&) = default;

   using DoZoomFunction = void (*)( AudacityProject *pProject,
       WaveTrack *pTrack,
       WaveTrackViewConstants::ZoomActions ZoomKind,
       const wxRect &rect, int zoomStart, int zoomEnd,
       bool fixedMousePoint);
   static void DoSpectrumZoom( AudacityProject *pProject,
       WaveTrack *pTrack,
       WaveTrackViewConstants::ZoomActions ZoomKind,
       const wxRect &rect, int zoomStart, int zoomEnd,
       bool fixedMousePoint);
   static void DoWaveformZoom( AudacityProject *pProject,
       WaveTrack *pTrack,
       WaveTrackViewConstants::ZoomActions ZoomKind,
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

private:

   // TrackPanelDrawable implementation
   void Draw(
      TrackPanelDrawingContext &context,
      const wxRect &rect, unsigned iPass ) override;

   wxRect DrawingArea(
      const wxRect &rect, const wxRect &panelRect, unsigned iPass ) override;

   std::weak_ptr<WaveTrack> mpTrack;

   int mZoomStart{}, mZoomEnd{};
   wxRect mRect{};
};

#endif
