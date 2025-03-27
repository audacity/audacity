/*
 * Audacity: A Digital Audio Editor
 */
#include "realtimeeffectviewerdialogmodel.h"
#include "libraries/lib-realtime-effects/RealtimeEffectState.h"
#include "libraries/lib-effects/EffectPlugin.h"
#include "libraries/lib-module-manager/PluginManager.h"
#include "trackedit/trackedittypes.h"
#include "trackedit/itrackeditproject.h"
#include "au3wrap/internal/wxtypes_convert.h"

namespace au::effects {
RealtimeEffectViewerDialogModel::RealtimeEffectViewerDialogModel(QObject* parent)
    : QObject(parent)
{
}

void RealtimeEffectViewerDialogModel::load()
{
    globalContext()->currentTrackeditProjectChanged().onNotify(this, [this]
    {
        subscribe();
    });
    subscribe();

    realtimeEffectService()->isActiveChanged().onReceive(this, [this](const RealtimeEffectStatePtr& state)
    {
        if (state->GetID() == m_stateId) {
            emit isActiveChanged();
        }
    });

    realtimeEffectService()->realtimeEffectRemoved().onReceive(this, [this](TrackId, const RealtimeEffectStatePtr& state)
    {
        if (state->GetID() == m_stateId) {
            unregisterState();
        }
    });

    realtimeEffectService()->realtimeEffectReplaced().onReceive(this, [this](TrackId, const RealtimeEffectStatePtr& oldState,
                                                                             const RealtimeEffectStatePtr&)
    {
        if (oldState->GetID() == m_stateId) {
            unregisterState();
        }
    });
}

bool RealtimeEffectViewerDialogModel::isVst3() const
{
    return m_isVst3;
}

bool RealtimeEffectViewerDialogModel::prop_isActive() const
{
    if (!m_stateId.has_value()) {
        return false;
    }
    return realtimeEffectService()->isActive(stateRegister()->stateById(*m_stateId));
}

void RealtimeEffectViewerDialogModel::prop_setIsActive(bool isActive)
{
    if (!m_stateId.has_value()) {
        return;
    }
    realtimeEffectService()->setIsActive(stateRegister()->stateById(*m_stateId), isActive);
}

RealtimeEffectStateId RealtimeEffectViewerDialogModel::prop_effectStateId() const
{
    return m_stateId.value_or(-1);
}

void RealtimeEffectViewerDialogModel::prop_setEffectStateId(RealtimeEffectStateId stateId)
{
    if (stateId == m_stateId) {
        return;
    }

    unregisterState();

    const auto state = stateRegister()->stateById(stateId);
    if (!state) {
        return;
    }

    m_stateId = stateId;
    const auto effectId = state->GetPluginID().ToStdString();
    const auto type = effectsProvider()->effectSymbol(effectId);
    const auto instance = std::dynamic_pointer_cast<effects::EffectInstance>(state->GetInstance());
    instancesRegister()->regInstance(muse::String::fromStdString(effectId), instance, state->GetAccess());

    const PluginDescriptor* const plug = PluginManager::Get().GetPlugin(effectId);
    if (!plug || !PluginManager::IsPluginAvailable(*plug)) {
        LOGE() << "plugin not available, effectId: " << effectId;
        return;
    }
    const std::string family = au3::wxToStdSting(plug->GetEffectFamily());
    m_isVst3 = family == "VST3";

    emit isActiveChanged();
    emit trackNameChanged();
    emit isMasterEffectChanged();
}

void RealtimeEffectViewerDialogModel::unregisterState()
{
    if (!m_stateId.has_value()) {
        return;
    }

    const auto state = stateRegister()->stateById(*m_stateId);
    IF_ASSERT_FAILED(state) {
        return;
    }
    const auto instance = std::dynamic_pointer_cast<effects::EffectInstance>(state->GetInstance());
    instancesRegister()->unregInstance(instance);
    m_stateId.reset();
}

QString RealtimeEffectViewerDialogModel::prop_trackName() const
{
    if (!m_stateId.has_value()) {
        return QString();
    }
    const auto trackName = realtimeEffectService()->effectTrackName(stateRegister()->stateById(*m_stateId));
    IF_ASSERT_FAILED(trackName.has_value()) {
        return QString();
    }

    return QString::fromStdString(*trackName);
}

void RealtimeEffectViewerDialogModel::subscribe()
{
    const trackedit::ITrackeditProjectPtr project = globalContext()->currentTrackeditProject();
    if (!project) {
        return;
    }
    project->trackChanged().onReceive(this, [this](const trackedit::Track& track) {
        IF_ASSERT_FAILED(m_stateId.has_value()) {
            return;
        }
        const std::optional<trackedit::TrackId> trackId = realtimeEffectService()->trackId(stateRegister()->stateById(*m_stateId));
        IF_ASSERT_FAILED(trackId.has_value()) {
            return;
        }

        if (track.id == trackId) {
            emit trackNameChanged();
        }
    });
}

bool RealtimeEffectViewerDialogModel::prop_isMasterEffect() const
{
    if (!m_stateId.has_value()) {
        return false;
    }
    return realtimeEffectService()->trackId(stateRegister()->stateById(*m_stateId)) == IRealtimeEffectService::masterTrackId;
}
}
