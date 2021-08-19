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
using TimeFreqBinsMap = std::map<long long, std::set<int>>;

class SpectralData{
private:
   double mSampleRate;
   int mWindowSize;
   long long mStartT;
   long long mEndT;

public:
    SpectralData(double sr)
    :mSampleRate(sr)
    // Set start and end in reverse for comparison during data addition
    ,mStartT(std::numeric_limits<long long>::max())
    ,mEndT( 0 )
    ,mWindowSize( 2048 )
    {}
    SpectralData(const SpectralData& src) = delete;

   TimeFreqBinsMap dataBuffer;
   std::vector<TimeFreqBinsMap> dataHistory;
   // TODO: replace with two pairs to save space
   std::vector<std::pair<int, int>> coordHistory;

   // Abstracted the copy method for future extension
   void CopyFrom(const std::shared_ptr<SpectralData> &src){
      mStartT = src->GetStartT();
      mEndT = src->GetEndT();

      // std containers will perform deepcopy automatically
      dataHistory = src->dataHistory;
      dataBuffer = src->dataBuffer;
      coordHistory = src->coordHistory;
   }

   int GetWindowSize() const{
      return mWindowSize;
   };

   double GetSR() const{
      return mSampleRate;
   }

   long long GetStartT() const{
      return mStartT;
   }

   long long GetEndT() const{
      return mEndT;
   }

   // The double time points is quantized into long long
   void addTimeFreqData(long long ll_sc, wxInt64 freq){
      // Update the start and end sampleCount of current selection
      if(ll_sc > mEndT)
         mEndT = ll_sc;
      if(ll_sc < mStartT)
         mStartT = ll_sc;

      // "Time-axis rounding"
      // Using int division to round the sampleCount to the hop size of FFT to save computation
      int hopSize = 128;
//      ll_sc = ll_sc / hopSize * hopSize;

      // "Frequency-bin rounding"
      // Round the exact frequency to the nearest bin
      int binNum = freq / (mSampleRate / mWindowSize);

      if(dataBuffer.find(ll_sc) == dataBuffer.end())
         dataBuffer[ll_sc] = std::set<int>{ binNum };
      else
         dataBuffer[ll_sc].insert(binNum);
   }

   void removeTimeFreqData(long long ll_sc, wxInt64 freq){
      int binNum = freq / mWindowSize;
      for(auto &dataBuf: dataHistory){
         if(dataBuf.find(ll_sc) != dataBuf.end()){
            dataBuf[ll_sc].erase(binNum);
         }
      }
   }

   void clearAllData(){
      // DataBuffer should be clear when the user release cursor
      dataHistory.clear();
      mStartT = std::numeric_limits<long long>::max();
      mEndT = 0;
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
   SpectrumView &operator=( const SpectrumView& ) = delete;

public:
   SpectrumView(WaveTrackView &waveTrackView, const SpectrumView &src) = delete;
   explicit SpectrumView(WaveTrackView &waveTrackView);
   ~SpectrumView() override;

   const Type &SubViewType() const override;

   std::shared_ptr<TrackVRulerControls> DoGetVRulerControls() override;

   std::shared_ptr<SpectralData> GetSpectralData();

   bool IsSpectral() const override;

   static int mBrushRadius;

   void CopyToSubView( WaveTrackSubView *destSubView ) const override;
private:
    std::weak_ptr<BrushHandle> mBrushHandle;
    std::shared_ptr<SpectralData> mpSpectralData;
    bool mOnBrushTool;

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
