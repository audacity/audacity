/*
 * Audacity: A Digital Audio Editor
 */
#include "realtimeeffectviewerdialogmodel.h"
#include "libraries/lib-realtime-effects/RealtimeEffectState.h"
#include "libraries/lib-effects/EffectPlugin.h"
#include "trackedit/trackedittypes.h"
#include "trackedit/itrackeditproject.h"

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
    dispatcher()->reg(this, "nav-trigger-control", [this]
    { dispatcher()->dispatch("play"); });

    realtimeEffectService()->isActiveChanged().onReceive(this, [this](EffectStateId stateId)
    {
        if (stateId == effectStateId()) {
            emit isActiveChanged();
        }
    });
}

bool RealtimeEffectViewerDialogModel::prop_isActive() const
{
    return realtimeEffectService()->isActive(effectStateId());
}

void RealtimeEffectViewerDialogModel::prop_setIsActive(bool isActive)
{
    realtimeEffectService()->setIsActive(effectStateId(), isActive);
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

    emit isActiveChanged();
    emit trackNameChanged();
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
    const std::optional<trackedit::TrackId> trackId = realtimeEffectService()->trackId(effectStateId());
    IF_ASSERT_FAILED(trackId.has_value()) {
        return {};
    }

    const trackedit::ITrackeditProjectPtr project = globalContext()->currentTrackeditProject();
    IF_ASSERT_FAILED(project) {
        return {};
    }

    const auto trackName = project->trackName(*trackId);
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
        const std::optional<trackedit::TrackId> trackId = realtimeEffectService()->trackId(effectStateId());
        IF_ASSERT_FAILED(trackId.has_value()) {
            return;
        }

        if (track.id == trackId) {
            emit trackNameChanged();
        }
    });
    project->trackRemoved().onReceive(this, [this](const trackedit::Track& track) {
        const std::optional<trackedit::TrackId> trackId = realtimeEffectService()->trackId(effectStateId());
        IF_ASSERT_FAILED(trackId.has_value()) {
            return;
        }

        if (track.id == trackId) {
            emit trackRemoved();
        }
    });
}

EffectStateId RealtimeEffectViewerDialogModel::effectStateId() const
{
    if (!m_effectState) {
        return 0;
    }
    return reinterpret_cast<EffectStateId>(m_effectState.get());
}
}
