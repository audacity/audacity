#pragma once

#include "au3-builtin-effects/PhaserBase.h"

bool operator==(const EffectPhaserSettings& a, const EffectPhaserSettings& b);
inline bool operator!=(const EffectPhaserSettings& a, const EffectPhaserSettings& b) { return !(a == b); }

namespace au::effects {
class PhaserEffect : public PhaserBase
{
public:
    PhaserEffect() = default;
    ~PhaserEffect() override = default;

    EffectGroup GetGroup() const override;

    using PhaserBase::Stages;
    using PhaserBase::DryWet;
    using PhaserBase::Freq;
    using PhaserBase::Phase;
    using PhaserBase::Depth;
    using PhaserBase::Feedback;
    using PhaserBase::OutGain;
};
}
