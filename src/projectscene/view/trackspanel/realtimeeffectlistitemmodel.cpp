/*
 * Audacity: A Digital Audio Editor
 */
#include "realtimeeffectlistitemmodel.h"

#include "au3-realtime-effects/RealtimeEffectState.h"

#include "framework/global/log.h"
#include "framework/global/translation.h"

namespace au::projectscene {
RealtimeEffectListItemModel::RealtimeEffectListItemModel(QObject* parent, effects::RealtimeEffectStatePtr effectState)
    : QObject{parent}, muse::Contextable(muse::iocCtxForQmlObject(this)), m_effectState{effectState}
{
    realtimeEffectService()->isActiveChanged().onReceive(this, [this](effects::RealtimeEffectStatePtr state)
    {
        if (state == m_effectState.lock()) {
            emit isActiveChanged();
        }
    });

    realtimeEffectService()->effectSettingsChanged().onNotify(this, [this]
    {
        emit isActiveChanged();
    });
}

RealtimeEffectListItemModel::~RealtimeEffectListItemModel()
{
    const auto state = m_effectState.lock();
    IF_ASSERT_FAILED(state) {
        // Effect state lifetime is expected to span more than this.
        return;
    }
    effectViewController()->hideEffect(state);
}

bool RealtimeEffectListItemModel::prop_isMasterEffect() const
{
    const auto state = m_effectState.lock();
    IF_ASSERT_FAILED(state) {
        return false;
    }
    return realtimeEffectService()->trackId(state) == effects::IRealtimeEffectService::masterTrackId;
}

bool RealtimeEffectListItemModel::prop_isAvailable() const
{
    return realtimeEffectService()->isAvailable(m_effectState.lock());
}

QString RealtimeEffectListItemModel::effectName() const
{
    const auto state = m_effectState.lock();
    IF_ASSERT_FAILED(state) {
        return QString();
    }

    const auto effectId = state->GetID().ToStdString();
    const auto name = effectsProvider()->effectName(effectId);
    const auto isValid = effectsProvider()->meta(muse::String::fromStdString(effectId)).isValid();

    if (!isValid) {
        //: %1 is the name of the effect that is missing/unavailable
        return muse::qtrc("effects", "Missing - %1").arg(name);
    }
    return QString::fromStdString(name);
}

QString RealtimeEffectListItemModel::effectState() const
{
    const auto state = m_effectState.lock();
    IF_ASSERT_FAILED(state) {
        return QString();
    }
    return QString::number(reinterpret_cast<uintptr_t>(state.get()));
}

effects::RealtimeEffectStatePtr RealtimeEffectListItemModel::effectStatePtr() const
{
    return m_effectState.lock();
}

void RealtimeEffectListItemModel::showEffectDialog()
{
    effectViewController()->showEffect(m_effectState.lock());
}

bool RealtimeEffectListItemModel::prop_isActive() const
{
    const auto state = m_effectState.lock();
    return realtimeEffectService()->isActive(state);
}

void RealtimeEffectListItemModel::prop_setIsActive(bool isActive)
{
    const auto state = m_effectState.lock();
    realtimeEffectService()->setIsActive(state, isActive);
}
}
