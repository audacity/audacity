/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "effects/effects_base/effectstypes.h"

#include "AmplifyBase.h"

#include "../common/params.h"

namespace au::effects {
class AmplifyEffect : public AmplifyBase
{
public:
    AmplifyEffect();

    // properties
    float peak() const;
    ratio_t defaultRatio() const;
    db_t defaultAmp() const;

    // params
    ratio_t ratio() const;
    Param<db_t> amp() const;      // dB
    void setAmp(db_t v);

    bool canClip() const;
    void setCanClip(bool v);

    // state
    bool isApplyAllowed() const;
};
}
