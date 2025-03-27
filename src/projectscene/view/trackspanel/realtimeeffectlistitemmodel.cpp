/*
 * Audacity: A Digital Audio Editor
 */
#include "realtimeeffectlistitemmodel.h"
#include "log.h"

namespace au::projectscene {
RealtimeEffectListItemModel::RealtimeEffectListItemModel(QObject* parent, effects::RealtimeEffectStateId stateId)
    : QObject{parent}, m_stateId{stateId}
{
    // Should be set already.
    assert(stateRegister()->stateById(m_stateId));

    realtimeEffectService()->isActiveChanged().onReceive(this, [this](effects::RealtimeEffectStatePtr state)
    {
        if (state == stateRegister()->stateById(m_stateId)) {
            emit isActiveChanged();
        }
    });
}

RealtimeEffectListItemModel::~RealtimeEffectListItemModel()
{
    const auto state = stateRegister()->stateById(m_stateId);
    if (!state) {
        return;
    }
    effectsProvider()->hideEffect(state);
}

bool RealtimeEffectListItemModel::prop_isMasterEffect() const
{
    const auto state = stateRegister()->stateById(m_stateId);
    if (!state) {
        return false;
    }
    return realtimeEffectService()->trackId(state) == effects::IRealtimeEffectService::masterTrackId;
}

QString RealtimeEffectListItemModel::effectName() const
{
    const auto state = stateRegister()->stateById(m_stateId);
    if (!state) {
        return QString();
    }
    return QString::fromStdString(effectsProvider()->effectName(*state));
}

effects::RealtimeEffectStateId RealtimeEffectListItemModel::effectStateId() const
{
    return m_stateId;
}

void RealtimeEffectListItemModel::toggleDialog()
{
    effectsProvider()->toggleShowEffect(stateRegister()->stateById(m_stateId));
}

bool RealtimeEffectListItemModel::prop_isActive() const
{
    return realtimeEffectService()->isActive(stateRegister()->stateById(m_stateId));
}

void RealtimeEffectListItemModel::prop_setIsActive(bool isActive)
{
    const auto state = stateRegister()->stateById(m_stateId);
    if (!state) {
        return;
    }
    realtimeEffectService()->setIsActive(stateRegister()->stateById(m_stateId), isActive);
}
}
