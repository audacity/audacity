/**********************************************************************

Audacity: A Digital Audio Editor

TimeShiftHandle.h

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_TIMESHIFT_HANDLE__
#define __AUDACITY_TIMESHIFT_HANDLE__

#include "../../UIHandle.h"

#include "../../MemoryX.h"

#include "../../Snap.h"
#include "../../Track.h"

#include "../../TrackPanel.h" // for ClipMoveState

struct HitTestResult;
class WaveClip;

class TimeShiftHandle final : public UIHandle
{
   TimeShiftHandle();
   TimeShiftHandle(const TimeShiftHandle&) = delete;
   TimeShiftHandle &operator=(const TimeShiftHandle&) = delete;
   static TimeShiftHandle& Instance();
   static HitTestPreview HitPreview
      (const AudacityProject *pProject, bool unsafe);

public:
   static HitTestResult HitAnywhere(const AudacityProject *pProject);
   static HitTestResult HitTest
      (const wxMouseEvent &event, const wxRect &rect, const AudacityProject *pProject);

   virtual ~TimeShiftHandle();

   Result Click
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override;

   Result Drag
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override;

   HitTestPreview Preview
      (const TrackPanelMouseEvent &event, const AudacityProject *pProject)
      override;

   Result Release
      (const TrackPanelMouseEvent &event, AudacityProject *pProject,
       wxWindow *pParent) override;

   Result Cancel(AudacityProject *pProject) override;

   void DrawExtras
      (DrawingPass pass,
       wxDC * dc, const wxRegion &, const wxRect &panelRect) override;

   bool StopsOnKeystroke() override { return true; }

private:
   Track *mCapturedTrack{};
   wxRect mRect{};

   bool mDidSlideVertically{};
   bool mSlideUpDownOnly{};

   bool mSnapPreferRightEdge{};

   int mMouseClickX{};

   // Handles snapping the selection boundaries or track boundaries to
   // line up with existing tracks or labels.  mSnapLeft and mSnapRight
   // are the horizontal index of pixels to display user feedback
   // guidelines so the user knows when such snapping is taking place.
   std::unique_ptr<SnapManager> mSnapManager{};

   ClipMoveState mClipMoveState{};
};

#endif
