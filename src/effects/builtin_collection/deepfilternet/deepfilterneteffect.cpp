/*
 * Audacity: A Digital Audio Editor
 */
#include "deepfilterneteffect.h"

#include "deepfilternet_backend.h"

#include <algorithm>
#include <cmath>
#include <deque>
#include <memory>
#include <string>
#include <vector>

#include "au3-effects/EffectOutputTracks.h"
#include "au3-command-parameters/ShuttleAutomation.h"
#include "au3-math/Resample.h"
#include "au3-strings/TranslatableString.h"
#include "au3-wave-track/WaveClip.h"
#include "au3-wave-track/WaveTrack.h"

namespace au::effects {
namespace {
constexpr double DeepFilterNetSampleRate = 48000.0;

struct DfnDeleter
{
    void operator()(dfn_handle* handle) const
    {
        dfn_destroy(handle);
    }
};

class DfnProcessor
{
public:
    explicit DfnProcessor(float attenuationLimitDb)
        : mHandle(dfn_create_default(1, attenuationLimitDb))
    {
        if (!mHandle) {
            mError = dfn_last_error();
            return;
        }

        mFrameLength = dfn_frame_length(mHandle.get());
        mDropSamples = dfn_delay_samples(mHandle.get());
        if (mFrameLength == 0) {
            mError = "DeepFilterNet returned an invalid frame length.";
            return;
        }

        mInputFrame.assign(mFrameLength, 0.0f);
        mOutputFrame.assign(mFrameLength, 0.0f);
    }

    bool isValid() const
    {
        return mHandle && !mInputFrame.empty();
    }

    const std::string& error() const
    {
        return mError;
    }

    bool processSamples(
        const float* samples, size_t count, size_t expectedOutputSamples, std::deque<float>& output)
    {
        for (size_t i = 0; i < count; ++i) {
            mInputFrame[mInputFill++] = samples[i];
            if (mInputFill == mFrameLength && !processFrame(expectedOutputSamples, output)) {
                return false;
            }
        }
        return true;
    }

    bool finish(size_t expectedOutputSamples, std::deque<float>& output)
    {
        if (mInputFill > 0) {
            std::fill(mInputFrame.begin() + mInputFill, mInputFrame.end(), 0.0f);
            if (!processFrame(expectedOutputSamples, output)) {
                return false;
            }
        }

        std::fill(mInputFrame.begin(), mInputFrame.end(), 0.0f);
        while (mOutputCount < expectedOutputSamples) {
            if (!processFrame(expectedOutputSamples, output)) {
                return false;
            }
        }

        return true;
    }

private:
    bool processFrame(size_t expectedOutputSamples, std::deque<float>& output)
    {
        if (dfn_process_frame(mHandle.get(), mInputFrame.data(), mOutputFrame.data()) != 0) {
            mError = dfn_last_error();
            return false;
        }

        mInputFill = 0;

        for (float sample : mOutputFrame) {
            if (mDropSamples > 0) {
                --mDropSamples;
                continue;
            }

            if (mOutputCount >= expectedOutputSamples) {
                break;
            }

            output.push_back(sample);
            ++mOutputCount;
        }

        return true;
    }

    std::unique_ptr<dfn_handle, DfnDeleter> mHandle;
    std::vector<float> mInputFrame;
    std::vector<float> mOutputFrame;
    size_t mFrameLength = 0;
    size_t mInputFill = 0;
    size_t mDropSamples = 0;
    size_t mOutputCount = 0;
    std::string mError;
};

std::vector<float> ResampleMono(
    const std::vector<float>& input, double inputRate, double outputRate, size_t expectedOutputSamples)
{
    if (input.empty()) {
        return {};
    }

    if (std::abs(inputRate - outputRate) < 0.5) {
        auto output = input;
        output.resize(expectedOutputSamples, 0.0f);
        return output;
    }

    const double factor = outputRate / inputRate;
    Resample resampler { true, factor, factor };

    std::vector<float> output(expectedOutputSamples + 4096);
    size_t consumed = 0;
    size_t produced = 0;

    while (consumed < input.size()) {
        if (produced == output.size()) {
            output.resize(output.size() + 4096);
        }

        const auto [used, created] = resampler.Process(
            factor,
            input.data() + consumed,
            input.size() - consumed,
            true,
            output.data() + produced,
            output.size() - produced);

        consumed += used;
        produced += created;

        if (used == 0 && created == 0) {
            output.resize(output.size() + 4096);
        }
    }

    output.resize(produced);
    output.resize(expectedOutputSamples, 0.0f);
    return output;
}

void AppendPending(std::deque<float>& pending, std::vector<float>& output)
{
    output.reserve(output.size() + pending.size());
    while (!pending.empty()) {
        output.push_back(pending.front());
        pending.pop_front();
    }
}
}

const ComponentInterfaceSymbol DeepFilterNetEffect::Symbol {
    TranslatableString("effects-deepfilternet", "DeepFilterNet noise reduction")
};

ComponentInterfaceSymbol DeepFilterNetEffect::GetSymbol() const
{
    return Symbol;
}

TranslatableString DeepFilterNetEffect::GetDescription() const
{
    return TranslatableString("effects-deepfilternet", "Reduces speech noise using DeepFilterNet3");
}

ManualPageID DeepFilterNetEffect::ManualPage() const
{
    return L"DeepFilterNet_Noise_Reduction";
}

EffectType DeepFilterNetEffect::GetType() const
{
    return EffectTypeProcess;
}

const EffectParameterMethods& DeepFilterNetEffect::Parameters() const
{
    static CapturedParameters<DeepFilterNetEffect, attenuationLimit, mix> parameters;
    return parameters;
}

bool DeepFilterNetEffect::CheckWhetherSkipEffect(const EffectSettings& s) const
{
    const auto& settings = GetSettings(s);
    return settings.mMix <= 0.000001 || settings.mAttenuationLimitDb <= 0.01;
}

bool DeepFilterNetEffect::Process(EffectInstance&, EffectSettings& s)
{
    const auto& settings = GetSettings(s);
    if (CheckWhetherSkipEffect(s)) {
        return true;
    }

    EffectOutputTracks outputs { *mTracks, GetType(), { { mT0, mT1 } } };
    bool success = true;

    int count = 0;
    for (auto track : outputs.Get().Selected<WaveTrack>()) {
        const double t0 = std::max(mT0, track->GetStartTime());
        const double t1 = std::min(mT1, track->GetEndTime());
        if (t1 <= t0) {
            count += track->NChannels();
            continue;
        }

        for (const auto channel : track->Channels()) {
            for (const auto clip : channel->Intervals()) {
                if (!clip->Intersects(t0, t1)) {
                    continue;
                }

                if (!ProcessOne(*clip, t0, t1, settings, count++)) {
                    success = false;
                    break;
                }
            }

            if (!success) {
                break;
            }
        }

        if (!success) {
            break;
        }
    }

    if (success) {
        outputs.Commit();
    }

    return success;
}

bool DeepFilterNetEffect::ProcessOne(
    WaveClipChannel& clip, double t0, double t1, const DeepFilterNetSettings& settings, int count)
{
    const auto clipStartTime = clip.GetPlayStartTime();
    const auto clipT0 = std::max(t0, clipStartTime);
    const auto clipT1 = std::min(t1, clip.GetPlayEndTime());
    if (clipT1 <= clipT0) {
        return true;
    }

    auto start = clip.TimeToSamples(clipT0 - clipStartTime);
    auto end = clip.TimeToSamples(clipT1 - clipStartTime);
    start = std::clamp(start, sampleCount { 0 }, clip.GetVisibleSampleCount());
    end = std::clamp(end, sampleCount { 0 }, clip.GetVisibleSampleCount());
    const auto len = end - start;
    const size_t totalSamples = len.as_size_t();

    if (totalSamples == 0) {
        return true;
    }

    std::vector<float> original(totalSamples);
    clip.GetSamples(reinterpret_cast<samplePtr>(original.data()), floatSample, start, totalSamples);

    const auto trackRate = static_cast<double>(clip.GetRate());
    const auto dfnSampleCount = static_cast<size_t>(
        std::max(1.0, std::round(double(totalSamples) * DeepFilterNetSampleRate / trackRate)));
    auto input48 = ResampleMono(original, trackRate, DeepFilterNetSampleRate, dfnSampleCount);

    if (TrackProgress(count, 0.05)) {
        return false;
    }

    DfnProcessor processor { static_cast<float>(settings.mAttenuationLimitDb) };
    if (!processor.isValid()) {
        mLastError = processor.error().empty()
                     ? TranslatableString("effects-deepfilternet", "Could not initialize DeepFilterNet.").translated().toStdString()
                     : processor.error();
        return false;
    }

    const auto wetMix = static_cast<float>(std::clamp(settings.mMix, 0.0, 1.0));
    std::deque<float> pending;
    std::vector<float> enhanced48;
    enhanced48.reserve(input48.size());

    size_t processed48 = 0;
    while (processed48 < input48.size()) {
        const size_t block = std::min<size_t>(8192, input48.size() - processed48);
        if (!processor.processSamples(input48.data() + processed48, block, input48.size(), pending)) {
            mLastError = processor.error();
            return false;
        }

        processed48 += block;
        AppendPending(pending, enhanced48);

        if (TrackProgress(count, 0.05 + 0.75 * double(processed48) / double(input48.size()))) {
            return false;
        }
    }

    if (!processor.finish(input48.size(), pending)) {
        mLastError = processor.error();
        return false;
    }
    AppendPending(pending, enhanced48);
    enhanced48.resize(input48.size(), 0.0f);

    auto processed = ResampleMono(enhanced48, DeepFilterNetSampleRate, trackRate, totalSamples);

    if (TrackProgress(count, 0.9)) {
        return false;
    }

    if (wetMix < 1.0f) {
        for (size_t i = 0; i < processed.size(); ++i) {
            processed[i] = original[i] * (1.0f - wetMix) + processed[i] * wetMix;
        }
    }

    size_t writtenSamples = 0;
    const auto effectiveFormat = clip.GetClip().GetSampleFormats().Effective();
    while (writtenSamples < totalSamples) {
        const size_t block = std::min<size_t>(8192, totalSamples - writtenSamples);

        clip.SetSamples(
            reinterpret_cast<constSamplePtr>(processed.data() + writtenSamples),
            floatSample,
            start + sampleCount(writtenSamples),
            block,
            effectiveFormat);

        writtenSamples += block;
        if (TrackProgress(count, 0.9 + 0.1 * double(writtenSamples) / double(totalSamples))) {
            return false;
        }
    }

    return true;
}
}
