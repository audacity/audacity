/**********************************************************************

Audacity: A Digital Audio Editor

TimeTrackVZoomHandle.h

Paul Licameli split from TimeTrackVZoomHandle.h

**********************************************************************/

#ifndef __AUDACITY_TIMETRACK_VZOOM_HANDLE__
#define __AUDACITY_TIMETRACK_VZOOM_HANDLE__

#include "../../../UIHandle.h" // to inherit

class TimeTrack;

class TimeTrackVZoomHandle final : public UIHandle
{
   TimeTrackVZoomHandle(const TimeTrackVZoomHandle&);

public:
   explicit TimeTrackVZoomHandle
      (const std::shared_ptr<TimeTrack> &pTrack, const wxRect &rect, int y);

   TimeTrackVZoomHandle &operator=(const TimeTrackVZoomHandle&) = default;

   ~TimeTrackVZoomHandle() override;

   void Enter( bool forward, AudacityProject * ) override;

   bool HandlesRightClick() override;

   Result Click
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override;

   Result Drag
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override;

   HitTestPreview Preview
      (const TrackPanelMouseState &state, AudacityProject *pProject)
      override;

   Result Release
      (const TrackPanelMouseEvent &event, AudacityProject *pProject,
       wxWindow *pParent) override;

   Result Cancel(AudacityProject *pProject) override;

private:
   std::weak_ptr<TimeTrack> mpTrack;
};

#endif
