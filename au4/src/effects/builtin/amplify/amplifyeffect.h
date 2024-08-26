/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "libraries/lib-builtin-effects/AmplifyBase.h"

namespace au::effects {
class AmplifyEffect : public ::AmplifyBase
{
public:
    AmplifyEffect();

    // params
    double ratio() const;
    void setRatio(double r);
};
}
