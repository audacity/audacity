#include "AmplifyBase.h"
#include "EffectOutputTracks.h"
#include "ShuttleAutomation.h"
#include "TimeStretching.h"
#include "WaveChannelUtilities.h"
#include "WaveTrack.h"

using namespace au::effects;

const EffectParameterMethods& AmplifyBase::Parameters() const
{
    static CapturedParameters<
        AmplifyBase,
        // Interactive case
        Ratio, Clipping>
    parameters;

    static CapturedParameters<AmplifyBase, Ratio> batchParameters {
        // If invoking Amplify from a macro, mCanClip is not a parameter
        // but is always true
        [](AmplifyBase&, EffectSettings&, AmplifyBase& e, bool) {
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
// AmplifyBase
//

const ComponentInterfaceSymbol AmplifyBase::Symbol { XO("Amplify") };

AmplifyBase::Instance::~Instance()
{
    // In case the dialog is cancelled before effect processing
    static_cast<AmplifyBase&>(GetEffect()).DestroyOutputTracks();
}

AmplifyBase::AmplifyBase()
{
    mAmp = Amp.def;
    // Ratio.def == DB_TO_LINEAR(Amp.def)
    Parameters().Reset(*this);
    mRatioClip = 0.0;
    mPeak = 0.0;

    SetLinearEffectFlag(true);
}

AmplifyBase::~AmplifyBase()
{
}

ComponentInterfaceSymbol AmplifyBase::GetSymbol() const
{
    return Symbol;
}

EffectType AmplifyBase::GetType() const
{
    return EffectTypeProcess;
}

unsigned AmplifyBase::GetAudioInCount() const
{
    return 1;
}

unsigned AmplifyBase::GetAudioOutCount() const
{
    return 1;
}

size_t AmplifyBase::ProcessBlock(
    EffectSettings&, const float* const* inBlock, float* const* outBlock,
    size_t blockLen)
{
    for (decltype(blockLen) i = 0; i < blockLen; i++) {
        outBlock[0][i] = inBlock[0][i] * mRatio;
    }

    return blockLen;
}

OptionalMessage AmplifyBase::LoadFactoryDefaults(EffectSettings& settings) const
{
    // To do: externalize state so const_cast isn't needed
    return const_cast<AmplifyBase&>(*this).DoLoadFactoryDefaults(settings);
}

OptionalMessage AmplifyBase::DoLoadFactoryDefaults(EffectSettings& /*settings*/)
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

bool AmplifyBase::Init()
{
    auto range = inputTracks()->Selected<const WaveTrack>();
    bool hasPitchOrSpeed
        =any_of(begin(range), end(range), [this](auto* pTrack) {
        return TimeStretching::HasPitchOrSpeed(*pTrack, mT0, mT1);
    });
    if (hasPitchOrSpeed) {
        range = MakeOutputTracks()->Get().Selected<const WaveTrack>();
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

std::any AmplifyBase::BeginPreview(const EffectSettings& /*settings*/)
{
    return { std::pair { CopyableValueRestorer(mRatio),
                         CopyableValueRestorer(mPeak) } };
}

void AmplifyBase::ClampRatio()
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
