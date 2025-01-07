/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "modularity/ioc.h"
#include "effects/effects_base/ieffectsprovider.h"

#include <QObject>

namespace au::projectscene {
class RealtimeEffectListItemModel : public QObject, public muse::Injectable
{
    Q_OBJECT

    muse::Inject<effects::IEffectsProvider> effectsProvider;

public:
    RealtimeEffectListItemModel(QObject* parent, effects::EffectStateId effectState);

    const effects::EffectStateId effectStateId;
    Q_INVOKABLE QString effectName() const;
    Q_INVOKABLE void showDialog();
};
}
