/**********************************************************************

  Audacity: A Digital Audio Editor

  SpectralDataManager.cpp

  Edward Hui

*******************************************************************//*!

\class SpectralDataManager
\brief Performs the calculation for spectral editing

*//*******************************************************************/

#include <iostream>
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
   bool mNeedOutput = true;
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

int SpectralDataManager::FindFrequencySnappingBin(WaveTrack *wt,
                                                      long long int startSC,
                                                      int hopSize,
                                                      double threshold,
                                                      int targetFreqBin)
{
   Setting setting;
   setting.mNeedOutput = false;
   Worker worker(setting);

   return worker.ProcessSnapping(wt, startSC, hopSize, setting.mWindowSize, threshold, targetFreqBin);
}

std::vector<int> SpectralDataManager::FindHighestFrequencyBins(WaveTrack *wt,
                                                          long long int startSC,
                                                          int hopSize,
                                                          double threshold,
                                                          int targetFreqBin)
 {
   Setting setting;
   setting.mNeedOutput = false;
   Worker worker(setting);

   return worker.ProcessOvertones(wt, startSC, hopSize, setting.mWindowSize, threshold, targetFreqBin);
 }

SpectralDataManager::Worker::Worker(const Setting &setting)
:TrackSpectrumTransformer{ setting.mNeedOutput, setting.mInWindowType, setting.mOutWindowType,
                           setting.mWindowSize, setting.mStepsPerWindow,
                           setting.mLeadingPadding, setting.mTrailingPadding}
// Work members
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
   const auto &hopSize = mpSpectralData->GetHopSize();
   auto startSample =  mpSpectralData->GetStartSample();
   const auto &endSample = mpSpectralData->GetEndSample();
   mStartHopNum = startSample / hopSize;
   mWindowCount = 0;

   startSample = std::max(static_cast<long long>(0), startSample - 2 * hopSize);
if (!TrackSpectrumTransformer::Process( Processor, wt, 1, startSample, endSample - startSample))
      return false;

   return true;
}

int SpectralDataManager::Worker::ProcessSnapping(WaveTrack *wt,
                                                  long long startSC,
                                                  int hopSize,
                                                  size_t winSize,
                                                  double threshold,
                                                  int targetFreqBin)
{
   mSnapThreshold = threshold;
   mSnapTargetFreqBin = targetFreqBin;
   mSnapSamplingRate = wt->GetRate();

   startSC = std::max(static_cast<long long>(0), startSC - 2 * hopSize);
   // The calculated frequency peak will be stored in mReturnFreq
   if (!TrackSpectrumTransformer::Process( SnappingProcessor, wt,
                                           1, startSC, winSize))
      return 0;

   return mSnapReturnFreqBin;
}

std::vector<int> SpectralDataManager::Worker::ProcessOvertones(WaveTrack *wt,
                                                 long long startSC,
                                                 int hopSize,
                                                 size_t winSize,
                                                 double threshold,
                                                 int targetFreqBin)
                                                 {
   mOvertonesThreshold = threshold;
   mSnapTargetFreqBin = targetFreqBin;
   mSnapSamplingRate = wt->GetRate();

   startSC = std::max(static_cast<long long>(0), startSC - 2 * hopSize);
   // The calculated multiple frequency peaks will be stored in mOvertonesTargetFreqBin
   TrackSpectrumTransformer::Process( OvertonesProcessor, wt, 1, startSC, winSize);
   return mOvertonesTargetFreqBin;
 }

bool SpectralDataManager::Worker::SnappingProcessor(SpectrumTransformer &transformer) {
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

      const double &sr = worker.mSnapSamplingRate;
      const double nyquistRate = sr / 2;
      const double &threshold = worker.mSnapThreshold;
      const double &spectrumSize = worker.mSpectrumSize;
      const int &targetBin = worker.mSnapTargetFreqBin;

      int binBound = spectrumSize * threshold;
      float maxValue = std::numeric_limits<float>::min();

      // Skip the first and last bin
      for(int i = -binBound; i < binBound; i++){
         int idx = std::clamp(targetBin + i, 0, static_cast<int>(spectrumSize - 1));
         if(record.mSpectrums[idx] > maxValue){
            maxValue = record.mSpectrums[idx];
            // Update the return frequency
            worker.mSnapReturnFreqBin = idx;
         }
      }
   }

   return true;
}

bool SpectralDataManager::Worker::OvertonesProcessor(SpectrumTransformer &transformer) {
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

      const double &spectrumSize = worker.mSpectrumSize;
      const int &targetBin = worker.mSnapTargetFreqBin;
      float maxValue = record.mSpectrums[targetBin];

      for(int i = 0; i < spectrumSize - 2; ++i){
         if(record.mSpectrums[i] > maxValue * worker.mOvertonesThreshold
         && (std::abs(targetBin - i % targetBin) % targetBin < 5 && i > targetBin))
            worker.mOvertonesTargetFreqBin.push_back(i);
      }
   }
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

   for(auto &spectralDataMap: mpSpectralData->dataHistory){
      // For all added frequency
      for(const int &freqBin: spectralDataMap[mStartHopNum]){
         record.mRealFFTs[freqBin] = 0;
         record.mImagFFTs[freqBin] = 0;
      }
   }

   mWindowCount++;
   mStartHopNum ++;
   return true;
}

auto SpectralDataManager::Worker::NewWindow(size_t windowSize)
-> std::unique_ptr<Window>
{
   return std::make_unique<MyWindow>(windowSize);
}

SpectralDataManager::Worker::MyWindow::~MyWindow() {

}
