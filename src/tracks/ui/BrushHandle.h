/**********************************************************************

Audacity: A Digital Audio Editor

BrushHandle.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_BRUSH_HANDLE__
#define __AUDACITY_BRUSH_HANDLE__

#include "../../UIHandle.h"
#include "../../SelectedRegion.h"
#include "../../Snap.h"

#include <vector>
#include <tracks/playabletrack/wavetrack/ui/SpectrumView.h>

class SelectionStateChanger;
class SnapManager;
class SpectrumAnalyst;
class Track;
class TrackView;
class TrackList;
class ViewInfo;
class WaveTrack;
class wxMouseState;

class AUDACITY_DLL_API BrushHandle : public UIHandle
{
   BrushHandle(const BrushHandle&);

public:
   explicit BrushHandle
      (const std::shared_ptr<TrackView> &pTrackView, bool useSnap,
       const TrackList &trackList,
       const TrackPanelMouseState &st, const ViewInfo &viewInfo,
       const std::shared_ptr<SpectralData> &mpSpectralData);

   // This always hits, but details of the hit vary with mouse position and
   // key state.
   static UIHandlePtr HitTest
      (std::weak_ptr<BrushHandle> &holder,
       const TrackPanelMouseState &state, const AudacityProject *pProject,
       const std::shared_ptr<TrackView> &pTrackView,
       const std::shared_ptr<SpectralData> &mpSpectralData);

   BrushHandle &operator=(const BrushHandle&) = default;
   
   virtual ~BrushHandle();

   bool IsClicked() const;

   void Enter(bool forward, AudacityProject *pProject) override;

   bool Escape(AudacityProject *pProject) override;

   void HandleTimeFreqData(long long ll_sc, wxInt64 freq);

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

   Result Cancel(AudacityProject*) override;

   static UIHandle::Result NeedChangeHighlight
      (const BrushHandle &oldState,
       const BrushHandle &newState);

   std::shared_ptr<SpectralData> mpSpectralData;

private:
   std::weak_ptr<Track> FindTrack();

   void Connect(AudacityProject *pProject);

   // TrackPanelDrawable implementation
   void Draw(
      TrackPanelDrawingContext &context,
      const wxRect &rect, unsigned iPass ) override;

   std::weak_ptr<TrackView> mpView;
   wxRect mRect{};
   SelectedRegion mInitialSelection{};

   std::shared_ptr<SnapManager> mSnapManager;
   SnapResults mSnapStart, mSnapEnd;
   bool mUseSnap{ true };
   bool mFreqSnapping { false };
   double mFreqSnappingRatio { 0.1 };

   long long mSampleCountUpperBound, mSampleCountLowerBound;
   wxInt64 mFreqUpperBound, mFreqLowerBound;
   int mMostRecentX{ -1 }, mMostRecentY{ -1 };

   bool mbCtrlDown;

   bool mAutoScrolling{};

   std::shared_ptr<SelectionStateChanger> mSelectionStateChanger;

   class TimerHandler;
   friend TimerHandler;
   std::shared_ptr<TimerHandler> mTimerHandler;
};
#endif
