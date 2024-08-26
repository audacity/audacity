/*
* Audacity: A Digital Audio Editor
*/
#include "amplifyeffect.h"

#include "log.h"

using namespace au::effects;

AmplifyEffect::AmplifyEffect()
{
}

double AmplifyEffect::ratio() const
{
    return mRatio;
}

void AmplifyEffect::setRatio(double r)
{
    mRatio = r;
    LOGDA() << "mRatio: " << r;
}
