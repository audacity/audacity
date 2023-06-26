#include "AudioContainer.h"
#include "TimeAndPitchInterface.h"


#include "StaffPad/TimeAndPitch.h"

class TIME_AND_PITCH_API StaffPadTimeAndPitch final :
    public TimeAndPitchInterface
{
public:
   StaffPadTimeAndPitch(
      size_t numChannels, TimeAndPitchSource&, const Parameters&);
   void GetSamples(float* const*, size_t) override;

private:
   void BootStretcher();
   const std::unique_ptr<staffpad::TimeAndPitch> mTimeAndPitch;
   TimeAndPitchSource& mAudioSource;
   AudioContainer mReadBuffer;
   const size_t mNumChannels;
   const double mTimeRatio;
};
