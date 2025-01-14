/*
 * Audacity: A Digital Audio Editor
 */
#include "realtimeeffectlistitemmodel.h"

namespace au::projectscene {
RealtimeEffectListItemModel::RealtimeEffectListItemModel(QObject* parent, effects::EffectStateId effectStateId)
    : QObject{parent}, effectStateId{effectStateId}
{
    realtimeEffectService()->isActiveChanged().onReceive(this, [this](effects::EffectStateId stateId)
    {
        if (stateId == this->effectStateId) {
            emit isActiveChanged();
        }
    });
}

QString RealtimeEffectListItemModel::effectName() const
{
    return QString::fromStdString(effectsProvider()->effectName(*reinterpret_cast<effects::RealtimeEffectState*>(effectStateId)));
}

void RealtimeEffectListItemModel::showDialog()
{
    effectsProvider()->showEffect(reinterpret_cast<effects::RealtimeEffectState*>(effectStateId));
}

bool RealtimeEffectListItemModel::prop_isActive() const
{
    return realtimeEffectService()->isActive(effectStateId);
}

void RealtimeEffectListItemModel::prop_setIsActive(bool isActive)
{
    realtimeEffectService()->setIsActive(effectStateId, isActive);
}
}
