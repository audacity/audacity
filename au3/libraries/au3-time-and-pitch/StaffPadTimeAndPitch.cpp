#include "StaffPadTimeAndPitch.h"
#include "DummyFormantShifterLogger.h"
#include "FormantShifterLogger.h"
#include "StaffPad/FourierTransform_pffft.h"
#include "TimeAndPitchExperimentalSettings.h"
#include <algorithm>
#include <cassert>
#include <cmath>
#include <memory>

namespace {
// Let's use StaffPad's default value. (We have to reproduce it here as it has
// to be specified in the `setup` call.)
constexpr auto maxBlockSize = 1024;

void GetOffsetBuffer(
    float** offsetBuffer, float* const* buffer, size_t numChannels,
    size_t offset)
{
    for (auto i = 0u; i < numChannels; ++i) {
        offsetBuffer[i] = buffer[i] + offset;
    }
}

int GetFftSize(int sampleRate, bool formantPreservationOn)
{
    if (
        const auto fftSize
            =TimeAndPitchExperimentalSettings::GetFftSizeOverride()) {
        return *fftSize;
    }

    // 44.1kHz maps to 4096 samples (i.e., 93ms) without formant preservation,
    // and 2048 with.
    // We grow the FFT size proportionally with the sample rate to keep the
    // window duration roughly constant, with quantization due to the
    // power-of-two constraint.
    // If needed some time in the future, we can decouple analysis window and
    // FFT sizes by zero-padding, allowing for very fine-grained window duration
    // without compromising performance.
    return 1 << (formantPreservationOn ? 11 : 12)
           + (int)std::round(std::log2(sampleRate / 44100.));
}

std::unique_ptr<staffpad::TimeAndPitch> CreateTimeAndPitch(
    int sampleRate, size_t numChannels,
    const TimeAndPitchInterface::Parameters& params, FormantShifter& shifter)
{
    const auto fftSize = GetFftSize(sampleRate, params.preserveFormants);
    auto shiftTimbreCb = params.preserveFormants && params.pitchRatio != 1.
                         ? [&](
        double factor, std::complex<float>* spectrum,
        const float* magnitude) {
        shifter.Process(magnitude, spectrum, factor);
    }
    : staffpad::TimeAndPitch::ShiftTimbreCb {};
    auto timeAndPitch = std::make_unique<staffpad::TimeAndPitch>(
        fftSize,
        TimeAndPitchExperimentalSettings::GetReduceImagingOverride().value_or(
            true),
        std::move(shiftTimbreCb));

    timeAndPitch->setup(static_cast<int>(numChannels), maxBlockSize);
    timeAndPitch->setTimeStretchAndPitchFactor(
        params.timeRatio, params.pitchRatio);

    return timeAndPitch;
}

std::unique_ptr<FormantShifterLoggerInterface>
GetFormantShifterLogger(int sampleRate)
{
    if (
        const auto logSample
            =TimeAndPitchExperimentalSettings::GetLogSample(sampleRate)) {
        return std::make_unique<FormantShifterLogger>(sampleRate, *logSample);
    }
    return std::make_unique<DummyFormantShifterLogger>();
}
} // namespace

StaffPadTimeAndPitch::StaffPadTimeAndPitch(
    int sampleRate, size_t numChannels, TimeAndPitchSource& audioSource,
    const Parameters& parameters)
    : mSampleRate(sampleRate)
    , mParameters(parameters)
    , mFormantShifterLogger(GetFormantShifterLogger(sampleRate))
    , mFormantShifter(
        sampleRate,
        TimeAndPitchExperimentalSettings::GetCutoffQuefrencyOverride()
        .value_or(0.002),
        *mFormantShifterLogger)
    , mAudioSource(audioSource)
    , mReadBuffer(maxBlockSize, numChannels)
    , mNumChannels(numChannels)
{
    if (mParameters.preserveFormants) {
        mFormantShifter.Reset(
            GetFftSize(sampleRate, parameters.preserveFormants));
    }
    if (
        !TimeAndPitchInterface::IsPassThroughMode(mParameters.timeRatio)
        ||// No need for sophisticated comparison for pitch ratio, as our UI doesn't
          // allow changes smaller than a cent.
        mParameters.pitchRatio != 1.) {
        InitializeStretcher();
    }
}

void StaffPadTimeAndPitch::GetSamples(float* const* output, size_t outputLen)
{
    if (!mTimeAndPitch) {
        // Pass-through
        return mAudioSource.Pull(output, outputLen);
    }

    auto numOutputSamples = 0u;
    while (numOutputSamples < outputLen)
    {
        if (IllState()) {
            for (auto i = 0u; i < mNumChannels; ++i) {
                std::fill_n(
                    output[i] + numOutputSamples, outputLen - numOutputSamples, 0.f);
            }
            return;
        }
        auto numOutputSamplesAvailable
            =mTimeAndPitch->getNumAvailableOutputSamples();
        while (numOutputSamplesAvailable <= 0)
        {
            auto numRequired = mTimeAndPitch->getSamplesToNextHop();
            while (numRequired > 0)
            {
                const auto numSamplesToFeed = std::min(numRequired, maxBlockSize);
                mAudioSource.Pull(mReadBuffer.Get(), numSamplesToFeed);
                mFormantShifterLogger->NewSamplesComing(numSamplesToFeed);
                mTimeAndPitch->feedAudio(mReadBuffer.Get(), numSamplesToFeed);
                numRequired -= numSamplesToFeed;
            }
            numOutputSamplesAvailable
                =mTimeAndPitch->getNumAvailableOutputSamples();
        }
        while (numOutputSamples < outputLen && numOutputSamplesAvailable > 0)
        {
            const auto numSamplesToGet
                =std::min({ maxBlockSize, numOutputSamplesAvailable,
                            static_cast<int>(outputLen - numOutputSamples) });
            // More-than-stereo isn't supported
            assert(mNumChannels <= 2);
            float* buffer[2] {};
            GetOffsetBuffer(buffer, output, mNumChannels, numOutputSamples);
            mTimeAndPitch->retrieveAudio(buffer, numSamplesToGet);
            numOutputSamplesAvailable -= numSamplesToGet;
            numOutputSamples += numSamplesToGet;
        }
    }
}

void StaffPadTimeAndPitch::OnCentShiftChange(int cents)
{
    mParameters.pitchRatio = std::pow(2., cents / 1200.);
    // If pitch shifing was zero before, now it isn't. If it was non-zero before,
    // we don't unset the stretcher even if the new pitch shift is zero, or
    // someone playing around with the effect could hear glitches.
    if (!mTimeAndPitch) {
        InitializeStretcher();
    } else {
        mTimeAndPitch->setTimeStretchAndPitchFactor(
            mParameters.timeRatio, mParameters.pitchRatio);
    }
}

void StaffPadTimeAndPitch::OnFormantPreservationChange(bool preserve)
{
    mParameters.preserveFormants = preserve;
    const auto fftSize = GetFftSize(mSampleRate, preserve);
    preserve ? mFormantShifter.Reset(fftSize) : mFormantShifter.Reset();
    // FFT size is a constant of the stretcher, so we need to reset it - if there
    // is a stretcher.
    if (mTimeAndPitch) {
        InitializeStretcher();
    }
}

void StaffPadTimeAndPitch::InitializeStretcher()
{
    mTimeAndPitch = CreateTimeAndPitch(
        mSampleRate, mNumChannels, mParameters, mFormantShifter);
    auto numOutputSamplesToDiscard
        =mTimeAndPitch->getLatencySamplesForStretchRatio(
              mParameters.timeRatio * mParameters.pitchRatio);
    AudioContainer container(maxBlockSize, mNumChannels);
    while (numOutputSamplesToDiscard > 0)
    {
        if (IllState()) {
            return;
        }
        auto numRequired = mTimeAndPitch->getSamplesToNextHop();
        while (numRequired > 0)
        {
            const auto numSamplesToFeed = std::min(maxBlockSize, numRequired);
            mAudioSource.Pull(container.Get(), numSamplesToFeed);
            mTimeAndPitch->feedAudio(container.Get(), numSamplesToFeed);
            numRequired -= numSamplesToFeed;
        }
        const auto totalNumSamplesToRetrieve = std::min(
            mTimeAndPitch->getNumAvailableOutputSamples(),
            numOutputSamplesToDiscard);
        auto totalNumRetrievedSamples = 0;
        while (totalNumRetrievedSamples < totalNumSamplesToRetrieve)
        {
            const auto numSamplesToRetrieve = std::min(
                maxBlockSize, totalNumSamplesToRetrieve - totalNumRetrievedSamples);
            mTimeAndPitch->retrieveAudio(container.Get(), numSamplesToRetrieve);
            totalNumRetrievedSamples += numSamplesToRetrieve;
        }
        numOutputSamplesToDiscard -= totalNumSamplesToRetrieve;
    }
}

bool StaffPadTimeAndPitch::IllState() const
{
    // It doesn't require samples, yet it doesn't have output samples available.
    // Note that this must not be a permanent state, and may recover if the user
    // changes the pitch shift.
    // TODO: try to fix this in the stretcher implementation.
    return mTimeAndPitch->getSamplesToNextHop() <= 0
           && mTimeAndPitch->getNumAvailableOutputSamples() <= 0;
}
