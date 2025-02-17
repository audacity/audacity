/**********************************************************************

  Audacity: A Digital Audio Editor

  Mix.cpp

  Dominic Mazzoni
  Markus Meyer
  Vaughan Johnson

*******************************************************************//**

\class Mixer
\brief Functions for doing the mixdown of the tracks.

*//*******************************************************************/
#include "Mix.h"
#include "MixerSource.h"

#include <cmath>
#include "EffectStage.h"
#include "Dither.h"
#include "Resample.h"
#include "WideSampleSequence.h"
#include "float_cast.h"
#include <numeric>

#include "DownmixSource.h"

namespace {
template<typename T, typename F> std::vector<T>
initVector(size_t dim1, const F& f)
{
    std::vector<T> result(dim1);
    for (auto& row : result) {
        f(row);
    }
    return result;
}

template<typename T> std::vector<std::vector<T> >
initVector(size_t dim1, size_t dim2)
{
    return initVector<std::vector<T> >(dim1,
                                       [dim2](auto& row){ row.resize(dim2); });
}
}

namespace {
void ConsiderStages(const Mixer::Stages& stages, size_t& blockSize)
{
    for (const auto& stage : stages) {
        // Need an instance to query acceptable block size
        const auto pInstance = stage.factory();
        if (pInstance) {
            blockSize = std::min(blockSize, pInstance->SetBlockSize(blockSize));
        }
        // Cache the first factory call
        stage.mpFirstInstance = move(pInstance);
    }
}

// Find a block size acceptable to all stages; side-effects on instances
size_t FindBufferSize(
    const Mixer::Inputs& inputs,
    const std::optional<Mixer::Stages>& masterEffects, size_t bufferSize)
{
    size_t blockSize = bufferSize;
    for (const auto& input : inputs) {
        const auto sequence = input.pSequence.get();
        const auto nInChannels = sequence->NChannels();
        if (!sequence) {
            assert(false);
            break;
        }
        ConsiderStages(input.stages, blockSize);
    }
    if (masterEffects) {
        ConsiderStages(*masterEffects, blockSize);
    }

    return blockSize;
}

auto NeedsDitherPred(const MixerOptions::StageSpecification& spec)
{
    return spec.mpFirstInstance && spec.mpFirstInstance->NeedsDither();
}
} // namespace

Mixer::Mixer(
    Inputs inputs, std::optional<Stages> masterEffects, const bool mayThrow, const WarpOptions& warpOptions, const double startTime,
    const double stopTime, const unsigned numOutChannels, const size_t outBufferSize, const bool outInterleaved, double outRate,
    sampleFormat outFormat, const bool highQuality, MixerSpec* const mixerSpec, ApplyVolume applyVolume)
    : mNumChannels{numOutChannels}
    , mInputs{move(inputs)}
    , mMasterEffects{move(masterEffects)}
    , mBufferSize{FindBufferSize(mInputs, mMasterEffects, outBufferSize)}
    , mApplyVolume{applyVolume}
    , mHighQuality{highQuality}
    , mFormat{outFormat}
    , mInterleaved{outInterleaved}
    , mTimesAndSpeed{std::make_shared<TimesAndSpeed>(TimesAndSpeed {
        startTime, stopTime, warpOptions.initialSpeed, startTime })}

    // non-interleaved
    , mTemp{mNumChannels, mBufferSize, 1, 1}
    , mBuffer{initVector<SampleBuffer>(
                  mInterleaved ? 1 : mNumChannels,
                  [format = mFormat,
                   size = mBufferSize * (mInterleaved ? mNumChannels : 1)](
                      auto& buffer) { buffer.Allocate(size, format); })},
    mEffectiveFormat { floatSample }
{
    assert(BufferSize() <= outBufferSize);
    const auto nChannelsIn
        =std::accumulate(mInputs.begin(), mInputs.end(), size_t {},
                         [](auto sum, const auto& input){
        return sum + input.pSequence->NChannels();
    });

    // Examine the temporary instances that were made in FindBufferSize
    // This finds a sufficient, but not necessary, condition to do dithering
    bool needsDither = std::any_of(mInputs.begin(), mInputs.end(),
                                   [](const Input& input){
        return std::any_of(input.stages.begin(), input.stages.end(), NeedsDitherPred);
    }
                                   );
    if (mMasterEffects) {
        needsDither |= std::any_of(
            mMasterEffects->begin(), mMasterEffects->end(), NeedsDitherPred);
    }

    auto pMixerSpec = (mixerSpec
                       && mixerSpec->GetNumChannels() == mNumChannels
                       && mixerSpec->GetNumTracks() == nChannelsIn
                       ) ? mixerSpec : nullptr;
    mHasMixerSpec = pMixerSpec != nullptr;

    // Reserve vectors first so we can take safe references to pushed elements
    mSources.reserve(nChannelsIn);
    // One stereo-capable stage per effect.
    const auto nMasterStages = mMasterEffects ? mMasterEffects->size() : 0;
    const auto nStages
        =std::accumulate(
              mInputs.begin(), mInputs.end(), 0,
              [](auto sum, const auto& input) {
        return sum + input.stages.size();
    })
          + nMasterStages;
    mSettings.reserve(nStages);
    mStageBuffers.reserve(nStages);

    size_t i = 0;
    std::vector<std::unique_ptr<DownmixSource> > downmixSources;
    for (auto& input : mInputs) {
        const auto& sequence = input.pSequence;
        if (!sequence) {
            assert(false);
            break;
        }

        auto& source = mSources.emplace_back(sequence, BufferSize(), outRate,
                                             warpOptions, highQuality, mayThrow, mTimesAndSpeed);
        AudioGraph::Source* pDownstream = &source;
        for (const auto& stage : input.stages) {
            if (
                auto& pNewDownstream
                    =RegisterEffectStage(*pDownstream, sequence->NChannels(), stage, outRate)) {
                pDownstream = pNewDownstream.get();
            }
        }
        downmixSources.emplace_back(
            std::make_unique<SequenceDownmixSource>(
                *pDownstream,
                *sequence,
                pMixerSpec ? &pMixerSpec->mMap[i] : nullptr
                )
            );

        i += sequence->NChannels();
    }

    if (mMasterEffects && !mMasterEffects->empty()) {
        mDownmixStage = std::make_unique<DownmixStage>(
            std::move(downmixSources), mNumChannels, mBufferSize, ApplyVolume::MapChannels
            );

        AudioGraph::Source* pDownstream = mDownmixStage.get();
        for (const auto& stage : *mMasterEffects) {
            if (
                auto& pNewDownstream
                    =RegisterEffectStage(*pDownstream, mNumChannels, stage, outRate)) {
                pDownstream = pNewDownstream.get();
            }
        }

        //Effect stage could have more than mNumChannels output
        //and expect at least 3 float buffers input to work correctly...
        std::vector<std::unique_ptr<DownmixSource> > masterDownmixSources;
        masterDownmixSources.push_back(std::make_unique<SimpleDonwmixSource>(*pDownstream, mNumChannels));
        mMasterDownmixStage = std::make_unique<DownmixStage>(
            std::move(masterDownmixSources), mNumChannels, mBufferSize, ApplyVolume::Mixdown);
        mDownstream = mMasterDownmixStage.get();
    } else {
        mDownmixStage = std::make_unique<DownmixStage>(
            std::move(downmixSources),
            mNumChannels,
            mBufferSize,
            mApplyVolume
            );
        mDownstream = mDownmixStage.get();
    }

    // Decide once at construction time
    std::tie(mNeedsDither, mEffectiveFormat) = NeedsDither(needsDither, outRate);
}

Mixer::~Mixer() = default;

std::pair<bool, sampleFormat>
Mixer::NeedsDither(bool needsDither, double rate) const
{
    // This will accumulate the widest effective format of any input
    // clip
    auto widestEffectiveFormat = narrowestSampleFormat;

    // needsDither may already be given as true.
    // There are many other possible disqualifiers for the avoidance of dither.
    if (std::any_of(mSources.begin(), mSources.end(),
                    std::mem_fn(&MixerSource::VariableRates))
        ) {
        // We will call MixVariableRates(), so we need nontrivial resampling
        needsDither = true;
    }

    for (const auto& input : mSources) {
        auto& sequence = input.GetSequence();

        if (sequence.GetRate() != rate) {
            // Also leads to MixVariableRates(), needs nontrivial resampling
            needsDither = true;
        } else if (mApplyVolume == ApplyVolume::Mixdown
                   && !mHasMixerSpec
                   && sequence.NChannels() > 1 && mNumChannels == 1) {
            needsDither = true;
        } else if (mApplyVolume != ApplyVolume::Discard) {
            /// TODO: more-than-two-channels
            for (auto c : { 0, 1 }) {
                const auto volume = sequence.GetChannelVolume(c);
                if (!(volume == 0.0 || volume == 1.0)) {
                    // Fractional volume may be applied even in MixSameRate
                    needsDither = true;
                }
            }
        }
        // Examine all tracks.  (This ignores the time bounds for the mixer.
        // If it did not, we might avoid dither in more cases.  But if we fix
        // that, remember that some mixers change their time bounds after
        // construction, as when scrubbing.)
        if (!sequence.HasTrivialEnvelope()) {
            // Varying or non-unit volume may be applied even in MixSameRate
            needsDither = true;
        }
        auto effectiveFormat = sequence.WidestEffectiveFormat();
        if (effectiveFormat > mFormat) {
            // Real, not just nominal, precision loss would happen in at
            // least one clip
            needsDither = true;
        }
        widestEffectiveFormat
            =std::max(widestEffectiveFormat, effectiveFormat);
    }

    if (needsDither) {
        // Results will be dithered to width mFormat
        return { true, mFormat };
    } else {
        // Results will not be dithered
        assert(widestEffectiveFormat <= mFormat);
        return { false, widestEffectiveFormat };
    }
}

void Mixer::Clear()
{
    for (auto c = 0; c < mTemp.Channels(); ++c) {
        mTemp.ClearBuffer(c, mBufferSize);
    }
}

size_t Mixer::Process(const size_t maxToProcess)
{
    assert(maxToProcess <= BufferSize());

    // MB: this is wrong! mT represented warped time, and mTime is too inaccurate to use
    // it here. It's also unnecessary I think.
    //if (mT >= mT1)
    //   return 0;

    auto&[mT0, mT1, _, mTime] = *mTimesAndSpeed;
    auto oldTime = mTime;
    // backwards (as possibly in scrubbing)
    const auto backwards = (mT0 > mT1);

    Clear();

    std::optional<size_t> maxOut;

    maxOut = mDownstream->Acquire(mTemp, maxToProcess);
    mDownstream->Release();

    if (!maxOut) {
        return 0;
    }

    if (backwards) {
        mTime = std::clamp(mTime, mT1, oldTime);
    } else {
        mTime = std::clamp(mTime, oldTime, mT1);
    }

    const auto dstStride = (mInterleaved ? mNumChannels : 1);
    auto ditherType = mNeedsDither
                      ? (mHighQuality ? gHighQualityDither : gLowQualityDither)
                      : DitherType::none;
    for (size_t c = 0; c < mNumChannels; ++c) {
        CopySamples(mTemp.GetReadPosition(c), floatSample,
                    (mInterleaved
                     ? mBuffer[0].ptr() + (c * SAMPLE_SIZE(mFormat))
                     : mBuffer[c].ptr()
                    ),
                    mFormat, *maxOut, ditherType,
                    1, dstStride);
    }

    // MB: this doesn't take warping into account, replaced with code based on mSamplePos
    //mT += (maxOut / mRate);

    assert(*maxOut <= maxToProcess);
    return *maxOut;
}

constSamplePtr Mixer::GetBuffer()
{
    return mBuffer[0].ptr();
}

constSamplePtr Mixer::GetBuffer(int channel)
{
    return mBuffer[channel].ptr();
}

sampleFormat Mixer::EffectiveFormat() const
{
    return mEffectiveFormat;
}

double Mixer::MixGetCurrentTime()
{
    return mTimesAndSpeed->mTime;
}

void Mixer::Reposition(double t, bool bSkipping)
{
    const auto&[mT0, mT1, _, __] = *mTimesAndSpeed;
    auto& mTime = mTimesAndSpeed->mTime;
    mTime = t;
    const bool backwards = (mT1 < mT0);
    if (backwards) {
        mTime = std::clamp(mTime, mT1, mT0);
    } else {
        mTime = std::clamp(mTime, mT0, mT1);
    }

    for (auto& source : mSources) {
        source.Reposition(mTime, bSkipping);
    }
}

void Mixer::SetTimesAndSpeed(double t0, double t1, double speed, bool bSkipping)
{
    wxASSERT(std::isfinite(speed));
    auto&[mT0, mT1, mSpeed, _] = *mTimesAndSpeed;
    mT0 = t0;
    mT1 = t1;
    mSpeed = fabs(speed);
    Reposition(t0, bSkipping);
}

void Mixer::SetSpeedForKeyboardScrubbing(double speed, double startTime)
{
    wxASSERT(std::isfinite(speed));
    auto&[mT0, mT1, mSpeed, _] = *mTimesAndSpeed;

    // Check if the direction has changed
    if ((speed > 0.0 && mT1 < mT0) || (speed < 0.0 && mT1 > mT0)) {
        // It's safe to use 0 and std::numeric_limits<double>::max(),
        // because Mixer::MixVariableRates() doesn't sample past the start
        // or end of the audio in a track.
        if (speed > 0.0 && mT1 < mT0) {
            mT0 = 0;
            mT1 = std::numeric_limits<double>::max();
        } else {
            mT0 = std::numeric_limits<double>::max();
            mT1 = 0;
        }

        Reposition(startTime, true);
    }

    mSpeed = fabs(speed);
}

std::unique_ptr<EffectStage>& Mixer::RegisterEffectStage(
    AudioGraph::Source& upstream, size_t numChannels,
    const MixerOptions::StageSpecification& stage,
    double outRate)
{
    // Make a mutable copy of stage.settings
    auto& settings = mSettings.emplace_back(stage.settings);
    // TODO: more-than-two-channels
    // Like mFloatBuffers but padding not needed for soxr
    // Allocate one extra buffer to hold dummy zero inputs
    // (Issue 3854)
    auto& stageInput = mStageBuffers.emplace_back(3, mBufferSize, 1);
    const auto& factory = [&stage] {
        // Avoid unnecessary repeated calls to the factory
        return stage.mpFirstInstance ? move(stage.mpFirstInstance)
               : stage.factory();
    };
    auto& pNewDownstream = mStages.emplace_back(EffectStage::Create(
                                                    -1, numChannels, upstream, stageInput, factory, settings, outRate,
                                                    std::nullopt));
    if (!pNewDownstream) {
        // Just omit the failed stage from rendering
        // TODO propagate the error?
        mStageBuffers.pop_back();
        mSettings.pop_back();
    }
    return pNewDownstream;
}
