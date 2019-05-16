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

class WaveClip;

struct ClipMoveState {
   // non-NULL only if click was in a WaveTrack and without Shift key:
   WaveClip *capturedClip {};

   bool capturedClipIsSelection {};
   TrackArray trackExclusions {};
   double hSlideAmount {};
   TrackClipArray capturedClipArray {};
   wxInt64 snapLeft { -1 }, snapRight { -1 };

   int mMouseClickX{};

   void clear()
   {
      capturedClip = nullptr;
      capturedClipIsSelection = false;
      trackExclusions.clear();
      hSlideAmount = 0;
      capturedClipArray.clear();
      snapLeft = snapRight = -1;
      mMouseClickX = 0;
   }
};

class TimeShiftHandle final : public UIHandle
{
   TimeShiftHandle(const TimeShiftHandle&) = delete;
   static HitTestPreview HitPreview
      (const AudacityProject *pProject, bool unsafe);

public:
   explicit TimeShiftHandle
   ( const std::shared_ptr<Track> &pTrack, bool gripHit );

   TimeShiftHandle &operator=(const TimeShiftHandle&) = default;

   bool IsGripHit() const { return mGripHit; }
   std::shared_ptr<Track> GetTrack() const { return mCapturedTrack; }

   // A utility function also used by menu commands
   static void CreateListOfCapturedClips
      ( ClipMoveState &state, const ViewInfo &viewInfo, Track &capturedTrack,
        TrackList &trackList, bool syncLocked, double clickTime );

   // A utility function also used by menu commands
   static void DoSlideHorizontal
      ( ClipMoveState &state, TrackList &trackList, Track &capturedTrack );

   // Try to move clips from one WaveTrack to another, before also moving
   // by some horizontal amount, which may be slightly adjusted to fit the
   // destination tracks.
   static bool DoSlideVertical
      ( ViewInfo &viewInfo, wxCoord xx,
        ClipMoveState &state, TrackList &trackList, Track &capturedTrack,
        Track &dstTrack, double &desiredSlideAmount );

   static UIHandlePtr HitAnywhere
      (std::weak_ptr<TimeShiftHandle> &holder,
       const std::shared_ptr<Track> &pTrack, bool gripHit);
   static UIHandlePtr HitTest
      (std::weak_ptr<TimeShiftHandle> &holder,
       const wxMouseState &state, const wxRect &rect,
       const std::shared_ptr<Track> &pTrack);

   virtual ~TimeShiftHandle();

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
       wxDC * dc, const wxRegion &, const wxRect &panelRect) override;

   bool StopsOnKeystroke() override { return true; }

private:
   std::shared_ptr<Track> mCapturedTrack;
   wxRect mRect{};

   bool mDidSlideVertically{};
   bool mSlideUpDownOnly{};

   bool mSnapPreferRightEdge{};

   // Handles snapping the selection boundaries or track boundaries to
   // line up with existing tracks or labels.  mSnapLeft and mSnapRight
   // are the horizontal index of pixels to display user feedback
   // guidelines so the user knows when such snapping is taking place.
   std::shared_ptr<SnapManager> mSnapManager{};

   ClipMoveState mClipMoveState{};
   bool mGripHit {};
};

#endif
