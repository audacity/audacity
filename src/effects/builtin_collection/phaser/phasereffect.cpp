#include "phasereffect.h"

#include "framework/global/types/number.h"

using namespace au::effects;

EffectGroup PhaserEffect::GetGroup() const
{
    return EffectGroup::DistortionAndModulation;
}

bool operator==(const EffectPhaserSettings& a, const EffectPhaserSettings& b)
{
    return a.mStages == b.mStages
           && a.mDryWet == b.mDryWet
           && muse::is_equal(a.mFreq, b.mFreq)
           && muse::is_equal(a.mPhase, b.mPhase)
           && a.mDepth == b.mDepth
           && a.mFeedback == b.mFeedback
           && muse::is_equal(a.mOutGain, b.mOutGain);
}
