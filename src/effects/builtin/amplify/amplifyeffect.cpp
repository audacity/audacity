#include "amplifyeffect.h"

#include "EffectOutputTracks.h"
#include "ShuttleAutomation.h"
#include "TimeStretching.h"
#include "WaveChannelUtilities.h"
#include "WaveTrack.h"

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
    mAmp = Amp.def;
    // Ratio.def == DB_TO_LINEAR(Amp.def)
    Parameters().Reset(*this);
    mRatioClip = 0.0;
    mPeak = 0.0;

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

db_t AmplifyEffect::defaultAmp() const
{
    return muse::linear_to_db(defaultRatio());
}

ratio_t AmplifyEffect::ratio() const
{
    return mRatio;
}

Param<db_t> AmplifyEffect::amp() const
{
    return make_param<db_t>(muse::linear_to_db(mRatio), muse::linear_to_db(Ratio.min), muse::linear_to_db(Ratio.max));
}

void AmplifyEffect::setAmp(db_t v)
{
    mRatio = std::clamp<ratio_t>(muse::db_to_linear(v), Ratio.min, Ratio.max);
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

EffectType AmplifyEffect::GetType() const
{
    return EffectTypeProcess;
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

    mRatioClip = 0.0;
    if (mPeak > 0.0) {
        mRatio = 1.0 / mPeak;
        mRatioClip = mRatio;
    } else {
        mRatio = 1.0;
    }
    mCanClip = false;

    ClampRatio();
    return { nullptr };
}

// Effect implementation

bool AmplifyEffect::Init()
{
    auto range = inputTracks()->Selected<const Au3WaveTrack>();
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
    return true;
}

std::any AmplifyEffect::BeginPreview(const EffectSettings& /*settings*/)
{
    return { std::pair { CopyableValueRestorer(mRatio),
                         CopyableValueRestorer(mPeak) } };
}

void AmplifyEffect::ClampRatio()
{
    // limit range of gain
    double dBInit = LINEAR_TO_DB(mRatio);
    double dB = std::clamp<double>(dBInit, Amp.min, Amp.max);
    if (dB != dBInit) {
        mRatio = DB_TO_LINEAR(dB);
    }

    mAmp = LINEAR_TO_DB(mRatio);
    mNewPeak = LINEAR_TO_DB(mRatio * mPeak);
}
