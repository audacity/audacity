/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  TimeAndPitchFactory.cpp

**********************************************************************/
#include "TimeAndPitchFactory.h"

#include "StaffPadTimeAndPitch.h"
#include "TimeStretchers.h"

namespace {
//! A speed-only stretcher, then StaffPad for the pitch shift
class ChainedTimeAndPitch final : public TimeAndPitchInterface
{
public:
    ChainedTimeAndPitch(
        int sampleRate, size_t numChannels,
        std::unique_ptr<TimeAndPitchSource> speedStage,
        const Parameters& parameters)
        : mSpeedStage{std::move(speedStage)}
        , mPitchStage{sampleRate, numChannels, *mSpeedStage, PitchOnly(parameters)}
    {
    }

    void GetSamples(float* const* output, size_t outputLen) override
    {
        mPitchStage.GetSamples(output, outputLen);
    }

    void OnCentShiftChange(int cents) override
    {
        mPitchStage.OnCentShiftChange(cents);
    }

    void OnFormantPreservationChange(bool preserve) override
    {
        mPitchStage.OnFormantPreservationChange(preserve);
    }

private:
    static Parameters PitchOnly(Parameters parameters)
    {
        parameters.timeRatio = 1.0;
        return parameters;
    }

    // Constructed first: the pitch stage pulls from it
    const std::unique_ptr<TimeAndPitchSource> mSpeedStage;
    StaffPadTimeAndPitch mPitchStage;
};

std::unique_ptr<TimeAndPitchSource> CreateSpeedStage(
    StretchMethod method, int sampleRate, size_t numChannels, TimeAndPitchSource& source,
    double timeRatio, long long numInputSamples)
{
    switch (method) {
    case StretchMethod::Varispeed:
        return TimeStretchers::CreateVarispeed(numChannels, source, timeRatio);
    case StretchMethod::Paulstretch:
        return TimeStretchers::CreatePaulstretch(sampleRate, numChannels, source, timeRatio);
    case StretchMethod::SoundTouch:
        return TimeStretchers::CreateSoundTouch(sampleRate, numChannels, source, timeRatio);
    case StretchMethod::Sbsms:
        return TimeStretchers::CreateSbsms(numChannels, source, timeRatio, numInputSamples);
    case StretchMethod::StaffPad:
        break;
    }
    return nullptr;
}
} // namespace

std::unique_ptr<TimeAndPitchInterface> CreateTimeAndPitchStretcher(
    StretchMethod method, int sampleRate, size_t numChannels, TimeAndPitchSource& source,
    const TimeAndPitchInterface::Parameters& parameters, long long numInputSamples)
{
    // Without a change of speed, every method comes down to StaffPad's pitch
    // shift, or to passing the audio through.
    if (method != StretchMethod::StaffPad
        && !TimeAndPitchInterface::IsPassThroughMode(parameters.timeRatio)) {
        // nullptr when the method isn't in this build: StaffPad stands in
        if (auto speedStage = CreateSpeedStage(
                method, sampleRate, numChannels, source, parameters.timeRatio, numInputSamples)) {
            return std::make_unique<ChainedTimeAndPitch>(
                sampleRate, numChannels, std::move(speedStage), parameters);
        }
    }
    return std::make_unique<StaffPadTimeAndPitch>(sampleRate, numChannels, source, parameters);
}
