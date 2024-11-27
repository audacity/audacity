/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "modularity/ioc.h"
#include "effects/effects_base/ieffectsprovider.h"
#include "realtimeeffectmenumodelbase.h"

namespace au::projectscene {
class RealtimeEffectMenuModel : public RealtimeEffectMenuModelBase
{
    Q_OBJECT

    muse::Inject<effects::IEffectsProvider> effectsProvider;

public:
    explicit RealtimeEffectMenuModel(QObject* parent = nullptr);

    Q_INVOKABLE void load() override;
};
}
