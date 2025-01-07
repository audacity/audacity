/*
 * Audacity: A Digital Audio Editor
 */
#include "realtimeeffectlistitemmodel.h"

namespace au::projectscene {
RealtimeEffectListItemModel::RealtimeEffectListItemModel(QObject* parent, effects::EffectStateId effectStateId)
    : QObject{parent}, effectStateId{effectStateId} {}

QString RealtimeEffectListItemModel::effectName() const
{
    return QString::fromStdString(effectsProvider()->effectName(*reinterpret_cast<effects::RealtimeEffectState*>(effectStateId)));
}

void RealtimeEffectListItemModel::showDialog()
{
    effectsProvider()->showEffect(reinterpret_cast<effects::RealtimeEffectState*>(effectStateId));
}
}
