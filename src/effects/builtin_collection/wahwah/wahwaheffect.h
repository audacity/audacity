#pragma once

#include "au3-builtin-effects/WahWahBase.h"

bool operator==(const EffectWahwahSettings& a, const EffectWahwahSettings& b);
inline bool operator!=(const EffectWahwahSettings& a, const EffectWahwahSettings& b) { return !(a == b); }

namespace au::effects {
class WahWahEffect : public WahWahBase
{
public:
    WahWahEffect() = default;
    ~WahWahEffect() override = default;

    EffectGroup GetGroup() const override;

    using WahWahBase::Freq;
    using WahWahBase::Phase;
    using WahWahBase::Depth;
    using WahWahBase::Res;
    using WahWahBase::FreqOfs;
    using WahWahBase::OutGain;
};
}
