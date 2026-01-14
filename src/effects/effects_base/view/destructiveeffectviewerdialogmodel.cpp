/*
 * Audacity: A Digital Audio Editor
 */
#include "destructiveeffectviewerdialogmodel.h"
#include "log.h"

namespace au::effects {
DestructiveEffectViewerDialogModel::DestructiveEffectViewerDialogModel(QObject* parent)
    : QObject(parent), muse::Injectable(muse::iocCtxForQmlObject(this))
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
    emit titleChanged();
    emit effectFamilyChanged();
    emit viewerComponentTypeChanged();
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
