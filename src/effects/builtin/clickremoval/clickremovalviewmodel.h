/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../common/abstracteffectmodel.h"

#include "effects/effects_base/ieffectsprovider.h"

class ClickRemovalBase;

namespace au::effects {
class ClickRemovalViewModel : public AbstractEffectModel
{
    Q_OBJECT

    muse::Inject<IEffectsProvider> effectsProvider;

public:
    ClickRemovalViewModel() = default;

private:
    void doReload() override;

    ClickRemovalBase* effect() const;
};
}
