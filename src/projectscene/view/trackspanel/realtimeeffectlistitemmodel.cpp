/*
 * Audacity: A Digital Audio Editor
 */
#include "realtimeeffectlistitemmodel.h"
#include "log.h"

namespace au::projectscene {
RealtimeEffectListItemModel::RealtimeEffectListItemModel(QObject* parent, effects::RealtimeEffectStatePtr effectState)
    : QObject{parent}, m_effectState{effectState}
{
    realtimeEffectService()->isActiveChanged().onReceive(this, [this](effects::RealtimeEffectStatePtr state)
    {
        if (state == m_effectState.lock()) {
            emit isActiveChanged();
        }
    });
}

RealtimeEffectListItemModel::~RealtimeEffectListItemModel()
{
    const auto state = m_effectState.lock();
    IF_ASSERT_FAILED(state) {
        // Effect state lifetime is expected to span more than this.
        return;
    }
    effectsProvider()->hideEffect(state);
}

bool RealtimeEffectListItemModel::prop_isMasterEffect() const
{
    const auto state = m_effectState.lock();
    IF_ASSERT_FAILED(state) {
        return false;
    }
    return realtimeEffectService()->trackId(state) == effects::IRealtimeEffectService::masterTrackId;
}

QString RealtimeEffectListItemModel::effectName() const
{
    const auto state = m_effectState.lock();
    IF_ASSERT_FAILED(state) {
        return QString();
    }
    return QString::fromStdString(effectsProvider()->effectName(*state));
}

effects::RealtimeEffectStatePtr RealtimeEffectListItemModel::effectState() const
{
    return m_effectState.lock();
}

void RealtimeEffectListItemModel::toggleDialog()
{
    effectsProvider()->toggleShowEffect(m_effectState.lock());
}

bool RealtimeEffectListItemModel::prop_isActive() const
{
    const auto state = m_effectState.lock();
    IF_ASSERT_FAILED(state) {
        return false;
    }
    return realtimeEffectService()->isActive(state);
}

void RealtimeEffectListItemModel::prop_setIsActive(bool isActive)
{
    const auto state = m_effectState.lock();
    IF_ASSERT_FAILED(state) {
        return;
    }
    realtimeEffectService()->setIsActive(state, isActive);
}
}
