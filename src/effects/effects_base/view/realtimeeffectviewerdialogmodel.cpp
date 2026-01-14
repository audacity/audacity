/*
 * Audacity: A Digital Audio Editor
 */
#include "realtimeeffectviewerdialogmodel.h"
#include "au3-realtime-effects/RealtimeEffectState.h"
#include "au3-effects/EffectPlugin.h"
#include "au3-module-manager/PluginManager.h"
#include "trackedit/trackedittypes.h"
#include "trackedit/itrackeditproject.h"
#include "au3wrap/internal/wxtypes_convert.h"

namespace au::effects {
RealtimeEffectViewerDialogModel::RealtimeEffectViewerDialogModel(QObject* parent)
    : QObject(parent), muse::Injectable(muse::iocCtxForQmlObject(this))
{
}

RealtimeEffectViewerDialogModel::~RealtimeEffectViewerDialogModel()
{
    unregisterState();
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

    configuration()->effectUIModeChanged().onNotify(this, [this] {
        emit useVendorUIChanged();
        emit viewerComponentTypeChanged();
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
    const std::string family = au3::wxToStdString(plug->GetEffectFamily());
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

bool RealtimeEffectViewerDialogModel::useVendorUI() const
{
    if (!m_effectState) {
        return true; // Default to vendor UI
    }

    const auto effectId = muse::String::fromStdString(m_effectState->GetID().ToStdString());
    if (effectId.empty()) {
        return true; // Default to vendor UI
    }

    const bool result = configuration()->effectUIMode(effectId) == EffectUIMode::VendorUI;
    return result;
}

void RealtimeEffectViewerDialogModel::refreshUIMode()
{
    emit useVendorUIChanged();
    emit viewerComponentTypeChanged();
}

ViewerComponentType RealtimeEffectViewerDialogModel::viewerComponentType() const
{
    const EffectFamily family = prop_effectFamily();

    // Audio Units always use AudioUnitViewer, which handles both vendor UI and Apple's generic UI internally
    if (family == EffectFamily::AudioUnit) {
        return ViewerComponentType::AudioUnit;
    }

    // Built-in effects always use their custom viewers
    if (family == EffectFamily::Builtin) {
        return ViewerComponentType::Builtin;
    }

    // For external plugins (VST3, LV2), check if we should use generated UI
    const bool shouldUseVendorUI = useVendorUI();
    if (!shouldUseVendorUI) {
        return ViewerComponentType::Generated;
    }

    // Use the appropriate vendor UI
    switch (family) {
    case EffectFamily::LV2:
        return ViewerComponentType::Lv2;
    case EffectFamily::VST3:
        return ViewerComponentType::Vst;
    default:
        return ViewerComponentType::Unknown;
    }
}
}
