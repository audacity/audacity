/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "libraries/lib-builtin-effects/AmplifyBase.h"

#include "../../effectstypes.h"

namespace au::effects {
class AmplifyEffect : public ::AmplifyBase
{
public:
    AmplifyEffect();

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
    ManualPageID ManualPage() const override;

    static EffectMeta meta();
};
}
