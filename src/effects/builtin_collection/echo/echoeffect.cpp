#include "echoeffect.h"

#include "framework/global/types/number.h"

using namespace au::effects;

std::shared_ptr<::EffectInstance> EchoEffect::MakeInstance() const
{
    return std::make_shared<Instance>(*this);
}

EffectGroup EchoEffect::GetGroup() const
{
    return EffectGroup::DelayAndReverb;
}

bool operator==(const EchoSettings& a, const EchoSettings& b)
{
    return muse::is_equal(a.delay, b.delay)
           && muse::is_equal(a.decay, b.decay);
}
