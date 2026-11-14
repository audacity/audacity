#include "basstrebleeffect.h"

#include "framework/global/types/number.h"

using namespace au::effects;

std::shared_ptr<::EffectInstance> BassTrebleEffect::MakeInstance() const
{
    return std::make_shared<Instance>(*this);
}

EffectGroup BassTrebleEffect::GetGroup() const
{
    return EffectGroup::EqAndFilters;
}

bool operator==(const BassTrebleSettings& a, const BassTrebleSettings& b)
{
    return muse::is_equal(a.mBass, b.mBass)
           && muse::is_equal(a.mTreble, b.mTreble)
           && muse::is_equal(a.mGain, b.mGain)
           && (a.mLink == b.mLink);
}
