/*
* Audacity: A Digital Audio Editor
*/
#include "amplifyeffect.h"

#include "libraries/lib-utility/MemoryX.h"

#include "global/types/number.h"

#include "log.h"

using namespace au::effects;

static double ampByRatio(double v)
{
    return LINEAR_TO_DB(v);
}

static double ampToRatio(double v)
{
    return DB_TO_LINEAR(v);
}

AmplifyEffect::AmplifyEffect()
{
}

double AmplifyEffect::peak() const
{
    return mPeak;
}

double AmplifyEffect::defaultRatio() const
{
    return 1.0 / mPeak;
}

double AmplifyEffect::defaultAmp() const
{
    return ampByRatio(defaultRatio());
}

double AmplifyEffect::ratio() const
{
    return mRatio;
}

Param<double> AmplifyEffect::amp() const
{
    return make_param<double>(ampByRatio(mRatio), ampByRatio(Ratio.min), ampByRatio(Ratio.max));
}

void AmplifyEffect::setAmp(double v)
{
    mRatio = std::clamp<double>(ampToRatio(v), Ratio.min, Ratio.max);
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

    double defRatio = defaultRatio();
    if (mRatio < defRatio || muse::is_equal(mRatio, defRatio)) {
        return true;
    }

    return false;
}
