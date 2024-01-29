#include "AudioContainer.h"
#include "TimeAndPitchInterface.h"

#include "StaffPad/TimeAndPitch.h"

#include <mutex>

class TIME_AND_PITCH_API StaffPadTimeAndPitch final :
    public TimeAndPitchInterface
{
public:
   StaffPadTimeAndPitch(
      int sampleRate, size_t numChannels, TimeAndPitchSource&,
      const Parameters&);
   void GetSamples(float* const*, size_t) override;

private:
   void BootStretcher();
   bool IllState() const;
   std::unique_ptr<staffpad::TimeAndPitch> mTimeAndPitch;
   TimeAndPitchSource& mAudioSource;
   AudioContainer mReadBuffer;
   const size_t mNumChannels;
   const double mTimeRatio;
   std::mutex mTimeAndPitchMutex;
};
