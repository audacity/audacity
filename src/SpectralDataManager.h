#include <tracks/playabletrack/wavetrack/ui/SpectrumView.h>
#include <effects/Effect.h>
#include "./SpectrumTransformer.h"

class SpectralDataManager{
public:
   SpectralDataManager();
   ~SpectralDataManager();
   static int ProcessTracks(TrackList& tracks);
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

protected:
   MyWindow &NthWindow(int nn) {
      return static_cast<MyWindow&>(Nth(nn));
   }
   std::unique_ptr<Window> NewWindow(size_t windowSize) override;
   bool DoStart() override;
   static bool Processor(SpectrumTransformer &transformer);
   bool DoFinish() override;

private:
   bool ApplyEffectToSelection();
   std::shared_ptr<SpectralData> mpSpectralData;
   int mWindowCount;
   unsigned long mStartSample;
   unsigned long mEndSample;
};