/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "modularity/ioc.h"
#include "effects/effects_base/ieffectsprovider.h"
#include "rteffectmenumodelbase.h"

namespace au::projectscene {
class RtEffectMenuModel : public RtEffectMenuModelBase
{
    Q_OBJECT

    muse::Inject<effects::IEffectsProvider> effectsProvider;

public:
    explicit RtEffectMenuModel(QObject* parent = nullptr);

    Q_INVOKABLE void load() override;
};
}
