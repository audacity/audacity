#include "wahwaheffect.h"

#include "framework/global/types/number.h"

using namespace au::effects;

EffectGroup WahWahEffect::GetGroup() const
{
    return EffectGroup::DistortionAndModulation;
}

bool operator==(const EffectWahwahSettings& a, const EffectWahwahSettings& b)
{
    return muse::is_equal(a.mFreq, b.mFreq)
           && muse::is_equal(a.mPhase, b.mPhase)
           && a.mDepth == b.mDepth
           && muse::is_equal(a.mRes, b.mRes)
           && a.mFreqOfs == b.mFreqOfs
           && muse::is_equal(a.mOutGain, b.mOutGain);
}
