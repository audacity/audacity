/**********************************************************************

Audacity: A Digital Audio Editor

SpectrumView.h

Paul Licameli split from WaveTrackView.h

**********************************************************************/

#ifndef __AUDACITY_SPECTRUM_VIEW__
#define __AUDACITY_SPECTRUM_VIEW__

#include <map>
#include <set>
#include "WaveTrackView.h" // to inherit


class WaveTrack;
class BrushHandle;
using TimeFreqBinsMap = std::map<long long, std::set<wxInt64>>;

class SpectralData{
private:
   double mSampleRate;

public:
    SpectralData(double sr)
    :mSampleRate(sr){}
    SpectralData(const SpectralData& src) = delete;

   TimeFreqBinsMap dataBuffer;
   std::vector<TimeFreqBinsMap> dataHistory;
   // TODO: replace with two pairs to save space
   std::vector<std::pair<int, int>> coordHistory;

   double GetSR() const{
      return mSampleRate;
   }

   // The double time points is quantized into long long
   void addTimeFreqData(long long ll_sc, wxInt64 freq){
      if(dataBuffer.find(ll_sc) == dataBuffer.end())
         dataBuffer[ll_sc] = std::set<wxInt64>{ freq };
      else
         dataBuffer[ll_sc].insert(freq);
   }

   void removeTimeFreqData(long long ll_sc, wxInt64 freq){
      for(auto &dataBuf: dataHistory){
         if(dataBuf.find(ll_sc) != dataBuf.end()){
            dataBuf[ll_sc].erase(freq);
         }
      }
   }

   // Using long long from the sample count, this function to convert it back to double time point
   // TODO: Clone from sampleCount, find better way to reuse the code there
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
   SpectrumView(WaveTrackView &waveTrackView, const SpectrumView &src) = delete;
   SpectrumView &operator=( const SpectrumView& ) = delete;

public:
   using WaveTrackSubView::WaveTrackSubView;
   SpectrumView(WaveTrackView &waveTrackView);
   ~SpectrumView() override;

   const Type &SubViewType() const override;

   std::shared_ptr<TrackVRulerControls> DoGetVRulerControls() override;

   std::shared_ptr<SpectralData> GetSpectralData();

   bool IsSpectral() const override;

   static int mBrushRadius;

   void CopyToSubView( WaveTrackSubView *destSubView ) const override;
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
      const WaveClip* selectedClip,
      const wxRect & rect );

   std::vector<UIHandlePtr> DetailedHitTest(
      const TrackPanelMouseState &state,
      const AudacityProject *pProject, int currentTool, bool bMultiTool )
      override;

protected:
   void DoSetMinimized( bool minimized ) override;
};

#endif
