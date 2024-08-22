/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "libraries/lib-builtin-effects/ToneGenBase.h"

#include "../../effectstypes.h"

namespace au::effects {
class ToneEffect : public ::ToneGenBase
{
public:
    ToneEffect();

    static const ComponentInterfaceSymbol Symbol;

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
    ManualPageID ManualPage() const override;

    static EffectMeta meta();
};
}
