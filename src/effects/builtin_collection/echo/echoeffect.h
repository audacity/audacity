#pragma once

#include "au3-builtin-effects/EchoBase.h"

bool operator==(const EchoSettings& a, const EchoSettings& b);
inline bool operator!=(const EchoSettings& a, const EchoSettings& b) { return !(a == b); }

namespace au::effects {
class EchoEffect : public EchoBase
{
public:
    EchoEffect() = default;
    ~EchoEffect() override = default;

    std::shared_ptr<::EffectInstance> MakeInstance() const override;

    EffectGroup GetGroup() const override;

    using EchoBase::Delay;
    using EchoBase::Decay;
};
}
