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
using TimeFreqBinsMap = std::unordered_map<long long, std::unordered_set<wxInt64>>;

class SpectralData{
private:
   double mSampleRate;

public:
    SpectralData(double sr)
    :mSampleRate(sr){}

   TimeFreqBinsMap dataBuffer;
   std::vector<TimeFreqBinsMap> dataHistory;
   // TODO: replace with two pairs to save space
   std::vector<std::pair<int, int>> coordHistory;

   // The double time points is quantized into long long
   void addFreqTimeData(wxInt64 freq, long long ll_sc){
      if(dataBuffer.find(ll_sc) == dataBuffer.end())
         dataBuffer[ll_sc] = std::unordered_set<long long>();
      else
         dataBuffer[ll_sc].insert(freq);
   }

   // Using long long from the sample count, this function to convert it back to double time point
   double scToTimeDouble(long long ll) const{
      return ll / mSampleRate;
   }

   void saveAndClearBuffer(){
      dataHistory.emplace_back(dataBuffer);
      dataBuffer.clear();
      coordHistory.clear();
   }
};

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

private:
    int mBrushSize;
    std::weak_ptr<BrushHandle> mBrushHandle;
    std::shared_ptr<SpectralData> mpSpectralData;

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
