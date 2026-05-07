/*
 * Audacity: A Digital Audio Editor
 */
#include "destructiveeffectviewerdialogmodel.h"

// TODO: au3 EffectInterface shouldn't be a dependency in Qt models
#include "au3-components/EffectInterface.h"

namespace au::effects {
DestructiveEffectViewerDialogModel::DestructiveEffectViewerDialogModel(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

void DestructiveEffectViewerDialogModel::load()
{
    configuration()->effectUIModeChanged().onNotify(this, [this] {
        emit useVendorUIChanged();
        emit viewerComponentTypeChanged();
    });
}

QString DestructiveEffectViewerDialogModel::title() const
{
    return m_title;
}

int DestructiveEffectViewerDialogModel::instanceId() const
{
    return m_instanceId;
}

void DestructiveEffectViewerDialogModel::setInstanceId(int newInstanceId)
{
    if (m_instanceId == newInstanceId) {
        return;
    }
    m_instanceId = newInstanceId;
    emit instanceIdChanged();

    m_effectId = instancesRegister()->effectIdByInstanceId(m_instanceId);
    const auto id = m_effectId.toStdString();
    m_title = QString::fromStdString(effectsProvider()->effectName(id));
    captureInitialSettings();
    emit titleChanged();
    emit effectFamilyChanged();
    emit viewerComponentTypeChanged();
}

void DestructiveEffectViewerDialogModel::rollbackSettings()
{
    if (!m_initialSettings) {
        return;
    }

    const EffectSettingsAccessPtr access = instancesRegister()->settingsAccessById(m_instanceId);
    if (!access) {
        return;
    }

    EffectSettings settings = *m_initialSettings;
    access->Set(std::move(settings));
    access->Flush();
    instancesRegister()->notifyAboutSettingsChanged(m_instanceId);
}

bool DestructiveEffectViewerDialogModel::useVendorUI() const
{
    if (m_effectId.empty()) {
        return true; // Default to vendor UI
    }
    const bool result = configuration()->effectUIMode(m_effectId) == EffectUIMode::VendorUI;
    return result;
}

void DestructiveEffectViewerDialogModel::refreshUIMode()
{
    emit useVendorUIChanged();
    emit viewerComponentTypeChanged();
}

EffectFamily DestructiveEffectViewerDialogModel::effectFamily() const
{
    if (m_effectId.empty()) {
        return EffectFamily::Unknown;
    }
    const EffectMeta meta = effectsProvider()->meta(m_effectId);
    return meta.family;
}

ViewerComponentType DestructiveEffectViewerDialogModel::viewerComponentType() const
{
    const EffectFamily family = effectFamily();

#ifdef Q_OS_MACOS
    // Audio Units always use AudioUnitViewer, which handles both vendor UI and Apple's generic UI internally
    if (family == EffectFamily::AudioUnit) {
        return ViewerComponentType::AudioUnit;
    }
#endif

    // Built-in effects always use their custom viewers
    if (family == EffectFamily::Builtin) {
        return ViewerComponentType::Builtin;
    }

    if (family == EffectFamily::Nyquist) {
        return ViewerComponentType::Generated;
    }

    // For external plugins (VST3, LV2), check if we should use generated UI
    const bool shouldUseVendorUI = useVendorUI();
    if (!shouldUseVendorUI) {
        return ViewerComponentType::Generated;
    }

    // Use the appropriate vendor UI
    switch (family) {
#ifdef Q_OS_LINUX
    case EffectFamily::LV2:
        return ViewerComponentType::Lv2;
#endif
    case EffectFamily::VST3:
        return ViewerComponentType::Vst;
    default:
        return ViewerComponentType::Unknown;
    }
}

void DestructiveEffectViewerDialogModel::captureInitialSettings()
{
    const EffectSettings* settings = instancesRegister()->settingsById(m_instanceId);
    if (!settings) {
        m_initialSettings.reset();
        return;
    }

    m_initialSettings = std::make_shared<EffectSettings>(*settings);
}
} // namespace au::effects
