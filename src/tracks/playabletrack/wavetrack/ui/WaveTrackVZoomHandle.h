/**********************************************************************

Audacity: A Digital Audio Editor

WaveTrackVZoomHandle.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_WAVE_TRACK_VZOOM_HANDLE__
#define __AUDACITY_WAVE_TRACK_VZOOM_HANDLE__

class wxMouseState;
class WaveTrack;
#include "../../../../UIHandle.h"


class WaveTrackVZoomHandle : public UIHandle
{
   WaveTrackVZoomHandle(const WaveTrackVZoomHandle&);
   static HitTestPreview HitPreview(const wxMouseState &state);

public:
   explicit WaveTrackVZoomHandle
      (const std::shared_ptr<WaveTrack> &pTrack, const wxRect &rect, int y);

   WaveTrackVZoomHandle &operator=(const WaveTrackVZoomHandle&) = default;

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
