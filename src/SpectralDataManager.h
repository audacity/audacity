/**********************************************************************

  Audacity: A Digital Audio Editor

  SpectralDataManager.h

  Edward Hui

*******************************************************************//*!

\class SpectralDataManager
\brief Performs the calculation for spectral editing

*//*******************************************************************/

#include "./SpectrumTransformer.h"
#include "effects/Effect.h"
#include "tracks/playabletrack/wavetrack/ui/SpectrumView.h"

class SpectralDataManager{
public:
   SpectralDataManager();
   ~SpectralDataManager();
   static int ProcessTracks(TrackList& tracks);
   static wxInt64 FindFrequencySnappingBin(WaveTrack *wt,
                                           long long startSC,
                                           double threshold,
                                           wxInt64 targetFreq);
private:
   class Worker;
   struct Setting;
};

class SpectralDataManager::Worker
      : public TrackSpectrumTransformer{
public:
   Worker(const Setting &setting);
   ~Worker();

   struct MyWindow : public Window
   {
      MyWindow(size_t windowSize)
            : Window{ windowSize }
            , mSpectrums(windowSize / 2 + 1)
            , mGains(windowSize / 2 + 1)
      {

      }
      ~MyWindow() override;

      FloatVector mSpectrums;
      FloatVector mGains;
   };

   bool Process(WaveTrack* wt, const std::shared_ptr<SpectralData> &sDataPtr);
   wxInt64 ProcessSnapping(WaveTrack *wt, long long int startSC, size_t winSize,
                           double threshold, wxInt64 targetFreq);

protected:
   MyWindow &NthWindow(int nn) {
      return static_cast<MyWindow&>(Nth(nn));
   }
   std::unique_ptr<Window> NewWindow(size_t windowSize) override;
   bool DoStart() override;
   static bool Processor(SpectrumTransformer &transformer);
   static bool SnappingProcessor(SpectrumTransformer &transformer);
   bool DoFinish() override;

private:
   bool ApplyEffectToSelection();
   std::shared_ptr<SpectralData> mpSpectralData;
   int mWindowCount;
   double mSnapSamplingRate;
   double mSnapThreshold;
   wxInt64 mSnapTargetFreq;
   wxInt64 mSnapReturnFreq { -1 };
   unsigned long mStartSample;
   unsigned long mEndSample;
};