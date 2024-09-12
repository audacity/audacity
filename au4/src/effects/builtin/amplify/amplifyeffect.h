/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "libraries/lib-builtin-effects/AmplifyBase.h"

#include "../common/params.h"

namespace au::effects {
class AmplifyEffect : public ::AmplifyBase
{
public:
    AmplifyEffect();

    // properties
    double peak() const;
    double defaultRatio() const;
    double defaultAmp() const;

    // params
    double ratio() const;
    Param<double> amp() const;      // dB
    void setAmp(double v);

    bool canClip() const;
    void setCanClip(bool v);

    // state
    bool isApplyAllowed() const;
};
}
