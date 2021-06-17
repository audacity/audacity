/**********************************************************************

Audacity: A Digital Audio Editor

SpectrumView.h

Paul Licameli split from WaveTrackView.h

**********************************************************************/

#ifndef __AUDACITY_SPECTRUM_VIEW__
#define __AUDACITY_SPECTRUM_VIEW__

#include <unordered_map>
#include "WaveTrackView.h" // to inherit


class WaveTrack;
class BrushHandle;

class SpectrumView final : public WaveTrackSubView
{
   SpectrumView( const SpectrumView& ) = delete;
   SpectrumView &operator=( const SpectrumView& ) = delete;

public:
   using WaveTrackSubView::WaveTrackSubView;
   SpectrumView(WaveTrackView &waveTrackView);
   ~SpectrumView() override;

   const Type &SubViewType() const override;

   std::shared_ptr<TrackVRulerControls> DoGetVRulerControls() override;

   bool IsSpectral() const override;

<<<<<<< HEAD
=======
   static std::unordered_map<wxInt64, std::unordered_set<double>> mFreqToTimePointsMap;
   static int mBrushRadius;

>>>>>>> e11212951e5c55216a2b22418d6b790b7ebb46a2
private:
    int mBrushSize;
    std::weak_ptr<BrushHandle> mBrushHandle;
    std::shared_ptr<std::unordered_map<wxInt64, std::vector<double>>> mpFreqToTimePointsMap;

   // TrackPanelDrawable implementation
   void Draw(
      TrackPanelDrawingContext &context,
      const wxRect &rect, unsigned iPass ) override;

   void DoDraw( TrackPanelDrawingContext &context,
      const WaveTrack *track,
      const wxRect & rect );

   std::vector<UIHandlePtr> DetailedHitTest(
      const TrackPanelMouseState &state,
      const AudacityProject *pProject, int currentTool, bool bMultiTool )
      override;

protected:
   void DoSetMinimized( bool minimized ) override;
};

#endif
