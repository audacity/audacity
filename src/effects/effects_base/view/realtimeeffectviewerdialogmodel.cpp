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

RealtimeEffectViewerDialogModel::~RealtimeEffectViewerDialogModel()
{
    unregisterState();
}

void RealtimeEffectViewerDialogModel::load()
{
    globalContext()->currentTrackeditProjectChanged().onNotify(this, [this]
    {
        subscribe();
    });
    subscribe();

    realtimeEffectService()->isActiveChanged().onReceive(this, [this](RealtimeEffectStatePtr stateId)
    {
        if (stateId == m_effectState) {
            emit isActiveChanged();
        }
    });

    realtimeEffectService()->effectSettingsChanged().onNotify(this, [this]{
        emit isActiveChanged();
    });
}

bool RealtimeEffectViewerDialogModel::isVst3() const
{
    return m_isVst3;
}

bool RealtimeEffectViewerDialogModel::prop_isActive() const
{
    return realtimeEffectService()->isActive(m_effectState);
}

void RealtimeEffectViewerDialogModel::prop_setIsActive(bool isActive)
{
    realtimeEffectService()->setIsActive(m_effectState, isActive);
}

QString RealtimeEffectViewerDialogModel::prop_effectState() const
{
    if (!m_effectState) {
        return {};
    }
    return QString::number(reinterpret_cast<uintptr_t>(m_effectState.get()));
}

void RealtimeEffectViewerDialogModel::prop_setEffectState(const QString& effectState)
{
    if (effectState == prop_effectState()) {
        return;
    }

    unregisterState();

    if (effectState.isEmpty()) {
        return;
    }

    m_effectState = reinterpret_cast<RealtimeEffectState*>(effectState.toULongLong())->shared_from_this();
    const auto effectId = m_effectState->GetID().ToStdString();
    const auto type = effectsProvider()->effectSymbol(effectId);
    const auto instance = std::dynamic_pointer_cast<effects::EffectInstance>(m_effectState->GetInstance());
    instancesRegister()->regInstance(muse::String::fromStdString(effectId), instance, m_effectState->GetAccess());

    const PluginDescriptor* const plug = PluginManager::Get().GetPlugin(effectId);
    if (!plug || !PluginManager::IsPluginAvailable(*plug)) {
        LOGE() << "plugin not available, effectId: " << effectId;
        return;
    }
    const std::string family = au3::wxToStdSting(plug->GetEffectFamily());
    m_isVst3 = family == "VST3";

    emit isActiveChanged();
    emit trackNameChanged();
    emit titleChanged();
    emit isMasterEffectChanged();
}

void RealtimeEffectViewerDialogModel::unregisterState()
{
    if (!m_effectState) {
        return;
    }

    const auto instance = std::dynamic_pointer_cast<effects::EffectInstance>(m_effectState->GetInstance());
    instancesRegister()->unregInstance(instance);
    m_effectState.reset();
}

QString RealtimeEffectViewerDialogModel::prop_trackName() const
{
    const auto trackName = realtimeEffectService()->effectTrackName(m_effectState);
    IF_ASSERT_FAILED(trackName.has_value()) {
        return QString();
    }

    return QString::fromStdString(*trackName);
}

QString RealtimeEffectViewerDialogModel::prop_title() const
{
    const auto effectName = realtimeEffectService()->effectName(m_effectState);
    return effectName.has_value() ? QString::fromStdString(*effectName) : QString();
}

void RealtimeEffectViewerDialogModel::subscribe()
{
    const trackedit::ITrackeditProjectPtr project = globalContext()->currentTrackeditProject();
    if (!project) {
        return;
    }
    project->trackChanged().onReceive(this, [this](const trackedit::Track& track) {
        const std::optional<trackedit::TrackId> trackId = realtimeEffectService()->trackId(m_effectState);
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
    return realtimeEffectService()->trackId(m_effectState) == IRealtimeEffectService::masterTrackId;
}
}
