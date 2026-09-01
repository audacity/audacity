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

    effectsProvider()->effectMetaListChanged().onNotify(this, [this]
    {
        emit availabilityChanged();
    });
}

RealtimeEffectListItemModel::~RealtimeEffectListItemModel()
{
    // After undo/redo the state may already be destroyed. Then no dialog can
    // be open for it (open dialogs share ownership), so there's nothing to hide.
    if (const auto state = m_effectState.lock()) {
        effectViewController()->hideEffect(state);
    }
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
    const auto state = m_effectState.lock();
    if (!state) {
        return false;
    }
    // Clickable only if the plugin is validated and ready to use THIS session;
    // a plugin validated only in a previous run must be re-validated first.
    return effectsProvider()->isEffectAvailable(muse::String::fromStdString(state->GetID().ToStdString()));
}

QString RealtimeEffectListItemModel::effectName() const
{
    const auto state = m_effectState.lock();
    IF_ASSERT_FAILED(state) {
        return QString();
    }

    const auto effectId = state->GetID().ToStdString();
    const auto name = QString::fromStdString(effectsProvider()->effectName(effectId));
    const auto meta = effectsProvider()->meta(muse::String::fromStdString(effectId));

    if (meta.isValid() && meta.isLoadable()) {
        return name;
    }

    return muse::qtrc("effects", "%1 - “%2”").arg(effects::pluginStateToString(meta.state)).arg(name);
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
