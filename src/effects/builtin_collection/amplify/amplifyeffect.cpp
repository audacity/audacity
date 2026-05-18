#include "amplifyeffect.h"

#include "framework/global/realfn.h"

#include "au3-effects/EffectOutputTracks.h"
#include "au3-command-parameters/ShuttleAutomation.h"
#include "au3-wave-track/TimeStretching.h"
#include "au3-wave-track/WaveChannelUtilities.h"
#include "au3-wave-track/WaveTrack.h"

#include "au3wrap/au3types.h"

using namespace au::effects;
using namespace au::au3;

const EffectParameterMethods& AmplifyEffect::Parameters() const
{
    static CapturedParameters<
        AmplifyEffect,
        // Interactive case
        Ratio, Clipping>
    parameters;

    static CapturedParameters<AmplifyEffect, Ratio> batchParameters {
        // If invoking Amplify from a macro, mCanClip is not a parameter
        // but is always true
        [](AmplifyEffect&, EffectSettings&, AmplifyEffect& e, bool) {
            e.mCanClip = true;
            return true;
        },
    };

    // Parameters differ depending on batch mode.  Option to disable clipping
    // is interactive only.
    if (IsBatchProcessing()) {
        return batchParameters;
    } else {
        return parameters;
    }
}

//
// AmplifyEffect
//

const ComponentInterfaceSymbol AmplifyEffect::Symbol { XO("Amplify") };

AmplifyEffect::Instance::~Instance()
{
    // In case the dialog is cancelled before effect processing
    static_cast<AmplifyEffect&>(GetEffect()).DestroyOutputTracks();
}

AmplifyEffect::AmplifyEffect()
{
    Parameters().Reset(*this);

    SetLinearEffectFlag(true);
}

AmplifyEffect::~AmplifyEffect()
{
}

ComponentInterfaceSymbol AmplifyEffect::GetSymbol() const
{
    return Symbol;
}

float AmplifyEffect::peak() const
{
    return static_cast<float>(mPeak);
}

ratio_t AmplifyEffect::defaultRatio() const
{
    return 1.0 / mPeak;
}

au::shared::Decibel AmplifyEffect::defaultAmp() const
{
    return au::shared::Decibel::fromLinear(defaultRatio());
}

ratio_t AmplifyEffect::ratio() const
{
    return mRatio;
}

Param<au::shared::Decibel> AmplifyEffect::amp() const
{
    return { au::shared::Decibel::fromLinear(mRatio), au::shared::Decibel::fromLinear(Ratio.min), au::shared::Decibel::fromLinear(Ratio.max) };
}

void AmplifyEffect::setAmp(au::shared::Decibel v)
{
    mRatio = std::clamp<ratio_t>(v.toLinear(), Ratio.min, Ratio.max);
}

bool AmplifyEffect::canClip() const
{
    return mCanClip;
}

void AmplifyEffect::setCanClip(bool v)
{
    mCanClip = v;
}

bool AmplifyEffect::isApplyAllowed() const
{
    if (mCanClip) {
        return true;
    }

    if (!(mPeak > 0.0)) {
        return false;
    }

    ratio_t defRatio = defaultRatio();
    if (mRatio < defRatio || muse::is_equal(mRatio, defRatio.to_double())) {
        return true;
    }

    return false;
}

::EffectType AmplifyEffect::GetType() const
{
    return EffectTypeProcess;
}

::EffectGroup AmplifyEffect::GetGroup() const
{
    return EffectGroup::VolumeAndCompression;
}

unsigned AmplifyEffect::GetAudioInCount() const
{
    return 1;
}

unsigned AmplifyEffect::GetAudioOutCount() const
{
    return 1;
}

size_t AmplifyEffect::ProcessBlock(
    EffectSettings&, const float* const* inBlock, float* const* outBlock,
    size_t blockLen)
{
    for (decltype(blockLen) i = 0; i < blockLen; i++) {
        outBlock[0][i] = inBlock[0][i] * mRatio;
    }

    return blockLen;
}

OptionalMessage AmplifyEffect::LoadFactoryDefaults(EffectSettings& settings) const
{
    // To do: externalize state so const_cast isn't needed
    return const_cast<AmplifyEffect&>(*this).DoLoadFactoryDefaults(settings);
}

OptionalMessage AmplifyEffect::DoLoadFactoryDefaults(EffectSettings& /*settings*/)
{
    Init();

    const auto newRatio = mPeak > 0.0 ? 1.0 / mPeak : 1.0;
    mCanClip = false;

    SetRatio(newRatio);
    return { nullptr };
}

// Effect implementation

bool AmplifyEffect::Init()
{
    auto range = inputTracks()->Selected<const Au3WaveTrack>();
    if (range.empty() || muse::RealIsEqualOrMore(mT0, mT1)) {
        mLastError = XO("No audio selected").Translation().ToStdString();
        return false;
    }
    bool hasPitchOrSpeed = any_of(begin(range), end(range), [this](auto* pTrack) {
        return TimeStretching::HasPitchOrSpeed(*pTrack, mT0, mT1);
    });
    if (hasPitchOrSpeed) {
        range = MakeOutputTracks()->Get().Selected<const Au3WaveTrack>();
    }
    mPeak = 0.0;
    for (auto t : range) {
        for (const auto& pChannel : t->Channels()) {
            auto pair = WaveChannelUtilities::GetMinMax(*pChannel, mT0, mT1); // may throw
            const float min = pair.first, max = pair.second;
            const float newpeak = std::max(fabs(min), fabs(max));
            mPeak = std::max<double>(mPeak, newpeak);
        }
    }
    return !muse::RealIsEqualOrMore(0.0, mPeak);
}

std::any AmplifyEffect::BeginPreview(const EffectSettings& /*settings*/)
{
    return { std::pair { CopyableValueRestorer(mRatio),
                         CopyableValueRestorer(mPeak) } };
}

void AmplifyEffect::SetRatio(double newRatio)
{
    mRatio = std::clamp<double>(newRatio, Ratio.min, Ratio.max);
}
