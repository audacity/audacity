/**********************************************************************

Audacity: A Digital Audio Editor

BrushHandle.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_BRUSH_HANDLE__
#define __AUDACITY_BRUSH_HANDLE__

#include <vector>
#include "ProjectSettings.h"
#include "tracks/playabletrack/wavetrack/ui/SpectrumView.h"
#include "../../UIHandle.h"
#include "../../SelectedRegion.h"

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
      (const std::shared_ptr<TrackView> &pTrackView,
       const TrackList &trackList,
       const TrackPanelMouseState &st, const ViewInfo &viewInfo,
       const std::shared_ptr<SpectralData> &pSpectralData,
       const ProjectSettings &pSettings);

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

   void HandleHopBinData(int hopNum, int freqBinNum);

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

   std::shared_ptr<SpectralData> mpSpectralData;

private:
   std::weak_ptr<Track> FindTrack();

   // TrackPanelDrawable implementation
   void Draw(
      TrackPanelDrawingContext &context,
      const wxRect &rect, unsigned iPass ) override;

   std::weak_ptr<TrackView> mpView;
   wxRect mRect{};

   // Example: For window size of 1024, with ratio 0.01
   // It searches for (+-) 1024*0.01 = 10 bins
   double mFreqSnappingRatio { 0.01 };

   bool mIsSmartSelection, mIsOvertones;
   long long mSampleCountUpperBound, mSampleCountLowerBound;
   wxInt64 mFreqUpperBound, mFreqLowerBound;
   int mMostRecentX{ -1 }, mMostRecentY{ -1 };
   int mBrushRadius;
   bool mbCtrlDown;

   std::shared_ptr<SelectionStateChanger> mSelectionStateChanger;
};
#endif
