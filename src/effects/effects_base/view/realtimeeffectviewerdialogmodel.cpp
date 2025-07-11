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
    restoreFocusToMainToolBar();
}

void RealtimeEffectViewerDialogModel::restoreFocusToMainToolBar()
{
    // At the time of writing, only when the "MainToolBar" panel (with the "Home", "Project", etc. tabs) is active
    // is the context resolved to "UiCtxProjectFocused". This should probably be reviewed and set to all sections
    // declared in `ProjectPage.qml` ("PlaybackSection", "TrackEffectsSection", etc.). Since that'd be a change of
    // potentially large impact, we leave it as is for now. Before 4.0 we will likely have to rework navigation anyway.
    const bool success = navigationController()->requestActivateByIndex("TopTool", "MainToolBar", { 0 });
    IF_ASSERT_FAILED(success) {
        LOGE() << "Failed to activate TopTool MainToolBar";
    }
}

bool RealtimeEffectViewerDialogModel::eventFilter(QObject* obj, QEvent* event)
{
    if (m_dialogView && obj == m_dialogView->window()) {
        switch (event->type()) {
        case QEvent::FocusIn:
            if (m_navigationPanel) {
                m_navigationPanel->requestActive();
            }
            break;
        case QEvent::FocusOut:
            restoreFocusToMainToolBar();
            break;
        }
    }
    return QObject::eventFilter(obj, event);
}

muse::ui::NavigationPanel* RealtimeEffectViewerDialogModel::prop_navigationPanel() const
{
    return m_navigationPanel;
}

void RealtimeEffectViewerDialogModel::prop_setNavigationPanel(muse::ui::NavigationPanel* navigationPanel)
{
    if (navigationPanel == m_navigationPanel) {
        return;
    }
    m_navigationPanel = navigationPanel;
    emit navigationPanelChanged();
}

void RealtimeEffectViewerDialogModel::prop_setDialogView(muse::uicomponents::DialogView* dialogView)
{
    if (dialogView == m_dialogView) {
        return;
    }
    m_dialogView = dialogView;
    emit dialogViewChanged();
}

muse::uicomponents::DialogView* RealtimeEffectViewerDialogModel::prop_dialogView() const
{
    return m_dialogView;
}

void RealtimeEffectViewerDialogModel::load()
{
    qApp->installEventFilter(this);

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

    realtimeEffectService()->effectSettingsChanged().onNotify(this, [this] {
        emit isActiveChanged();
    });
}

EffectFamily RealtimeEffectViewerDialogModel::prop_effectFamily() const
{
    if (!m_effectState) {
        return EffectFamily::Unknown;
    }
    const auto effectId = m_effectState->GetID().ToStdString();
    const PluginDescriptor* const plug = PluginManager::Get().GetPlugin(effectId);
    if (!plug || !PluginManager::IsPluginAvailable(*plug)) {
        return EffectFamily::Unknown;
    }
    const std::string family = au3::wxToStdSting(plug->GetEffectFamily());
    if (family == "VST3") {
        return EffectFamily::VST3;
    } else if (family == "LV2") {
        return EffectFamily::LV2;
    } else if (family == "AudioUnit") {
        return EffectFamily::AudioUnit;
    } else if (family == "Audacity") {
        return EffectFamily::Builtin;
    } else {
        assert(false);
        return EffectFamily::Unknown;
    }
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

    emit isActiveChanged();
    emit trackNameChanged();
    emit titleChanged();
    emit isMasterEffectChanged();
    emit effectFamilyChanged();
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
