/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../common/abstracteffectmodel.h"

#include "effects/effects_base/ieffectsprovider.h"

namespace au::effects {
class ClickRemovalEffect;
class ClickRemovalViewModel : public AbstractEffectModel
{
    Q_OBJECT

    muse::Inject<IEffectsProvider> effectsProvider;

public:
    ClickRemovalViewModel() = default;

private:
    void doReload() override;

    ClickRemovalEffect* effect() const;
};
}
