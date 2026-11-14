#include "AudioContainer.h"
#include "FormantShifter.h"
#include "FormantShifterLoggerInterface.h"
#include "StaffPad/TimeAndPitch.h"
#include "TimeAndPitchInterface.h"

class TIME_AND_PITCH_API StaffPadTimeAndPitch final : public TimeAndPitchInterface
{
public:
    StaffPadTimeAndPitch(
        int sampleRate, size_t numChannels, TimeAndPitchSource&, const Parameters&);
    void GetSamples(float* const*, size_t) override;
    void OnCentShiftChange(int cents) override;
    void OnFormantPreservationChange(bool preserve) override;

private:
    bool IllState() const;
    void InitializeStretcher();

    const int mSampleRate;
    const std::unique_ptr<FormantShifterLoggerInterface> mFormantShifterLogger;
    TimeAndPitchInterface::Parameters mParameters;
    FormantShifter mFormantShifter;
    std::unique_ptr<staffpad::TimeAndPitch> mTimeAndPitch;
    TimeAndPitchSource& mAudioSource;
    AudioContainer mReadBuffer;
    const size_t mNumChannels;
};
