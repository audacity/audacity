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
#include "au3-strings/TranslatableString.h"
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
        if (std::abs(track->GetRate() - DeepFilterNetSampleRate) > 0.5) {
            mLastError = TranslatableString(
                "effects-deepfilternet",
                "DeepFilterNet noise reduction currently requires 48000 Hz audio.").translated().toStdString();
            return false;
        }

        const double t0 = std::max(mT0, track->GetStartTime());
        const double t1 = std::min(mT1, track->GetEndTime());
        if (t1 <= t0) {
            count += track->NChannels();
            continue;
        }

        for (const auto channel : track->Channels()) {
            if (!ProcessOne(*channel, t0, t1, settings, count++)) {
                success = false;
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
    WaveChannel& channel, double t0, double t1, const DeepFilterNetSettings& settings, int count)
{
    const auto start = channel.TimeToLongSamples(t0);
    const auto end = channel.TimeToLongSamples(t1);
    const auto len = end - start;
    const size_t totalSamples = len.as_size_t();

    if (totalSamples == 0) {
        return true;
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
    std::vector<float> inputBuffer(channel.GetMaxBlockSize());
    std::vector<float> dryBuffer;
    std::vector<float> outputBuffer;
    size_t readSamples = 0;
    size_t writtenSamples = 0;

    const auto flushPending = [&](bool force) {
        while (!pending.empty() && (force || pending.size() >= channel.GetMaxBlockSize())) {
            const size_t writeLen = force
                                    ? std::min(pending.size(), totalSamples - writtenSamples)
                                    : std::min(channel.GetMaxBlockSize(), totalSamples - writtenSamples);
            if (writeLen == 0) {
                break;
            }

            outputBuffer.resize(writeLen);
            for (size_t i = 0; i < writeLen; ++i) {
                outputBuffer[i] = pending.front();
                pending.pop_front();
            }

            if (wetMix < 1.0f) {
                dryBuffer.resize(writeLen);
                channel.GetFloats(dryBuffer.data(), start + sampleCount(writtenSamples), writeLen);
                for (size_t i = 0; i < writeLen; ++i) {
                    outputBuffer[i] = dryBuffer[i] * (1.0f - wetMix) + outputBuffer[i] * wetMix;
                }
            }

            if (!channel.SetFloats(outputBuffer.data(), start + sampleCount(writtenSamples), writeLen)) {
                return false;
            }

            writtenSamples += writeLen;
            if (TrackProgress(count, double(writtenSamples) / double(totalSamples))) {
                return false;
            }
        }
        return true;
    };

    while (readSamples < totalSamples) {
        const auto pos = start + sampleCount(readSamples);
        const size_t block = limitSampleBufferSize(
            channel.GetBestBlockSize(pos), sampleCount(totalSamples - readSamples));

        channel.GetFloats(inputBuffer.data(), pos, block);
        if (!processor.processSamples(inputBuffer.data(), block, totalSamples, pending)) {
            mLastError = processor.error();
            return false;
        }

        readSamples += block;
        if (!flushPending(false)) {
            return false;
        }
    }

    if (!processor.finish(totalSamples, pending)) {
        mLastError = processor.error();
        return false;
    }

    return flushPending(true);
}
}
