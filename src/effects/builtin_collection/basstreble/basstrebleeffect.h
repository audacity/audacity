#pragma once

#include "au3-builtin-effects/BassTrebleBase.h"

bool operator==(const BassTrebleSettings& a, const BassTrebleSettings& b);
inline bool operator!=(const BassTrebleSettings& a, const BassTrebleSettings& b) { return !(a == b); }

namespace au::effects {
class BassTrebleEffect : public BassTrebleBase
{
public:
    BassTrebleEffect() = default;
    ~BassTrebleEffect() override = default;

    std::shared_ptr<::EffectInstance> MakeInstance() const override;

    EffectGroup GetGroup() const override;

    using BassTrebleBase::Bass;
    using BassTrebleBase::Treble;
    using BassTrebleBase::Gain;
    using BassTrebleBase::Link;
};
}
