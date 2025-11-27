/*
 * Audacity: A Digital Audio Editor
 */
#include "effectviewerdialogmodel.h"
#include "log.h"

namespace au::effects {
EffectViewerDialogModel::EffectViewerDialogModel(QObject* parent)
    : QObject(parent)
{
}

void EffectViewerDialogModel::load()
{
    configuration()->pluginUIModeChanged().onNotify(this, [this] {
        emit useVendorUIChanged();
        emit viewerComponentTypeChanged();
    });
}

QString EffectViewerDialogModel::title() const
{
    return m_title;
}

int EffectViewerDialogModel::instanceId() const
{
    return m_instanceId;
}

void EffectViewerDialogModel::setInstanceId(int newInstanceId)
{
    if (m_instanceId == newInstanceId) {
        return;
    }
    m_instanceId = newInstanceId;
    emit instanceIdChanged();

    m_effectId = instancesRegister()->effectIdByInstanceId(m_instanceId);
    const auto id = m_effectId.toStdString();
    m_title = QString::fromStdString(effectsProvider()->effectName(id));
    emit titleChanged();
    emit effectFamilyChanged();
    emit viewerComponentTypeChanged();
}

bool EffectViewerDialogModel::useVendorUI() const
{
    if (m_effectId.empty()) {
        return true; // Default to vendor UI
    }
    const bool result = configuration()->pluginUIMode(m_effectId) == PluginUIMode::VendorUI;
    return result;
}

void EffectViewerDialogModel::refreshUIMode()
{
    emit useVendorUIChanged();
    emit viewerComponentTypeChanged();
}

EffectFamily EffectViewerDialogModel::effectFamily() const
{
    if (m_effectId.empty()) {
        return EffectFamily::Unknown;
    }
    const EffectMeta meta = effectsProvider()->meta(m_effectId);
    return meta.family;
}

ViewerComponentType EffectViewerDialogModel::viewerComponentType() const
{
    const EffectFamily family = effectFamily();

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
} // namespace au::effects
