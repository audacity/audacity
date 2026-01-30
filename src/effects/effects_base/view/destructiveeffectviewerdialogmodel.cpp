/*
 * Audacity: A Digital Audio Editor
 */
#include "destructiveeffectviewerdialogmodel.h"

#include "effects/nyquist/internal/nyquistparameterextractorservice.h"

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

    // Nyquist effects: check if it's the Nyquist Prompt
    if (family == EffectFamily::Nyquist) {
        // Use the parameter extractor to check if this is the Nyquist Prompt
        // The Nyquist Prompt is a special effect that needs a custom UI (text editor)
        // instead of the auto-generated parameter UI
        IParameterExtractorService* extractor = parameterExtractorRegistry()
                                                ? parameterExtractorRegistry()->extractorForFamily(EffectFamily::Nyquist)
                                                : nullptr;
        if (extractor) {
            EffectInstance* instance = instancesRegister()->instanceById(m_instanceId).get();
            if (instance) {
                // Cast to NyquistParameterExtractorService to access isNyquistPrompt()
                // We know this is safe because we got it from the registry for Nyquist family
                auto* nyquistExtractor = dynamic_cast<au::effects::NyquistParameterExtractorService*>(extractor);
                if (nyquistExtractor && nyquistExtractor->isNyquistPrompt(instance)) {
                    return ViewerComponentType::NyquistPrompt;
                }
            }
        }
        // Regular Nyquist effects use generated UI (no vendor UI available)
        return ViewerComponentType::Generated;
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
