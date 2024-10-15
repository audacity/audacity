/*
* Audacity: A Digital Audio Editor
*/
#include "amplifyeffect.h"

#include "log.h"

using namespace au::effects;

AmplifyEffect::AmplifyEffect()
{
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
