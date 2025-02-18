/*
 * Audacity: A Digital Audio Editor
 */
#include "realtimeeffectlistitemmodel.h"

namespace au::projectscene {
RealtimeEffectListItemModel::RealtimeEffectListItemModel(QObject* parent, effects::RealtimeEffectStatePtr effectState)
    : QObject{parent}, m_effectState{std::move(effectState)}
{
    realtimeEffectService()->isActiveChanged().onReceive(this, [this](effects::RealtimeEffectStatePtr state)
    {
        if (state == this->m_effectState) {
            emit isActiveChanged();
        }
    });
}

RealtimeEffectListItemModel::~RealtimeEffectListItemModel()
{
    effectsProvider()->hideEffect(m_effectState);
}

bool RealtimeEffectListItemModel::prop_isMasterEffect() const
{
    return realtimeEffectService()->trackId(m_effectState) == effects::IRealtimeEffectService::masterTrackId;
}

QString RealtimeEffectListItemModel::effectName() const
{
    return QString::fromStdString(effectsProvider()->effectName(*m_effectState));
}

effects::RealtimeEffectStatePtr RealtimeEffectListItemModel::effectState() const
{
    return m_effectState;
}

void RealtimeEffectListItemModel::toggleDialog()
{
    effectsProvider()->toggleShowEffect(m_effectState);
}

bool RealtimeEffectListItemModel::prop_isActive() const
{
    return realtimeEffectService()->isActive(m_effectState);
}

void RealtimeEffectListItemModel::prop_setIsActive(bool isActive)
{
    realtimeEffectService()->setIsActive(m_effectState, isActive);
}
}
