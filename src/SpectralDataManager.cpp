#include "FFT.h"
#include "SpectralDataManager.h"
#include "WaveTrack.h"

SpectralDataManager::SpectralDataManager()= default;

SpectralDataManager::~SpectralDataManager()= default;

struct SpectralDataManager::Setting{
   eWindowFunctions mInWindowType = eWinFuncHann;
   eWindowFunctions mOutWindowType = eWinFuncHann;
   size_t mWindowSize = 2048;
   unsigned mStepsPerWindow = 4;
   bool mLeadingPadding = true;
   bool mTrailingPadding = true;
};

int SpectralDataManager::ProcessTracks(TrackList &tracks){
   int applyCount = 0;
   Setting setting;
   Worker worker(setting);

   for ( auto wt : tracks.Any< WaveTrack >() ) {
      auto &trackView = TrackView::Get(*wt);

      if(auto waveTrackViewPtr = dynamic_cast<WaveTrackView*>(&trackView)){
         for(const auto &subViewPtr : waveTrackViewPtr->GetAllSubViews()){
            if(!subViewPtr->IsSpectral())
               continue;
            auto sView = std::static_pointer_cast<SpectrumView>(subViewPtr).get();
            auto pSpectralData = sView->GetSpectralData();

            if(!pSpectralData->dataHistory.empty()){
               worker.Process(wt, pSpectralData);
               applyCount += static_cast<int>(pSpectralData->dataHistory.size());
               pSpectralData->clearAllData();
            }
         }
      }
   }

   return applyCount;
}

SpectralDataManager::Worker::Worker(const Setting &setting)
:TrackSpectrumTransformer{ true, setting.mInWindowType, setting.mOutWindowType,
                           setting.mWindowSize, setting.mStepsPerWindow,
                           setting.mLeadingPadding, setting.mTrailingPadding}
// Work members
, mWindowCount { 0 }
, mStartSample { 0 }
, mEndSample { 0 }
{
}

SpectralDataManager::Worker::~Worker() = default;

bool SpectralDataManager::Worker::DoStart() {
   return TrackSpectrumTransformer::DoStart();
}
bool SpectralDataManager::Worker::DoFinish() {
   return TrackSpectrumTransformer::DoFinish();
}

bool SpectralDataManager::Worker::Process(WaveTrack* wt,
                                          const std::shared_ptr<SpectralData>& pSpectralData)
{
   mpSpectralData = pSpectralData;
   auto &dataHistory = mpSpectralData->dataHistory;
   mStartSample = mpSpectralData->GetStartT();
   mEndSample = mStartSample + mWindowSize;

   if (!TrackSpectrumTransformer::Process( Processor, wt, 1,
                                           mpSpectralData->GetStartT(),
                                           mpSpectralData->GetEndT() - mpSpectralData->GetStartT()))
      return false;

   return true;
}

bool SpectralDataManager::Worker::Processor(SpectrumTransformer &transformer)
{
   auto &worker = static_cast<Worker &>(transformer);
   // Compute power spectrum in the newest window
   {
      MyWindow &record = worker.NthWindow(0);
      float *pSpectrum = &record.mSpectrums[0];
      const double dc = record.mRealFFTs[0];
      *pSpectrum++ = dc * dc;
      float *pReal = &record.mRealFFTs[1], *pImag = &record.mImagFFTs[1];
      for (size_t nn = worker.mSpectrumSize - 2; nn--;) {
         const double re = *pReal++, im = *pImag++;
         *pSpectrum++ = re * re + im * im;
      }
      const double nyquist = record.mImagFFTs[0];
      *pSpectrum = nyquist * nyquist;
   }

   worker.ApplyEffectToSelection();
   return true;
}

bool SpectralDataManager::Worker::ApplyEffectToSelection() {
   auto &record = NthWindow(0);
   auto nyquist = mpSpectralData->GetSR() / 2;

   for(const auto &spectralDataMap: mpSpectralData->dataHistory){
      for(const auto &data: spectralDataMap){
         long long sc = data.first;
         // Check if the data's time sampleCount within this window
         if(sc >= mStartSample && sc < mEndSample){
            // For all added frequency
            for(const wxInt64 &selectedFreq: data.second){
               double dtargetBin = selectedFreq / nyquist * mSpectrumSize;
               int targetBin = static_cast<int>(dtargetBin);
               record.mRealFFTs[targetBin] = 0;
               record.mImagFFTs[targetBin] = 0;
               record.mRealFFTs[targetBin - 1] = 0;
               record.mImagFFTs[targetBin - 1] = 0;
               record.mRealFFTs[targetBin + 1] = 0;
               record.mImagFFTs[targetBin + 1] = 0;
            }
         }
      }
   }

//   record.mRealFFTs[0] *= (record.mGains[0] - 1.0);
//   // The Fs/2 component is stored as the imaginary part of the DC component
//   record.mImagFFTs[0] *= (record.mGains[last] - 1.0);

   // Slide for next window
   mWindowCount++;
   mStartSample += mStepSize;
   mEndSample += mStepSize;
   return true;
}

auto SpectralDataManager::Worker::NewWindow(size_t windowSize)
-> std::unique_ptr<Window>
{
   return std::make_unique<MyWindow>(windowSize);
}

SpectralDataManager::Worker::MyWindow::~MyWindow() {

}
