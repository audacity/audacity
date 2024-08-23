/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "libraries/lib-builtin-effects/AmplifyBase.h"

#include "../../effectstypes.h"

#include "../../tmpconcept/ieffectdialog.h"
#include "modularity/ioc.h"
#include "global/iinteractive.h"

namespace au::effects {
class AmplifyEffect : public ::AmplifyBase, public IEffectDialog
{
    muse::Inject<muse::IInteractive> interactive;

public:
    AmplifyEffect();

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
    ManualPageID ManualPage() const override;

    // IEffectDialog
    bool show() override;

    // params
    double ratio() const;
    void setRatio(double r);

    // meta
    static EffectMeta meta();
};
}
