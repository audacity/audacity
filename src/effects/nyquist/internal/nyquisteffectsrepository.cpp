/*
* Audacity: A Digital Audio Editor
*/
#include "nyquisteffectsrepository.h"

#include "effects/effects_base/internal/effectsutils.h"
#include "effects/effects_base/internal/au3/au3effectsutils.h"
#include "effects/effects_base/effectstypes.h"
#include "au3wrap/internal/wxtypes_convert.h"

#include "au3-module-manager/PluginManager.h"
#include "au3-effects/EffectManager.h"
#include "spectrogram/spectrogramtypes.h"

au::effects::NyquistEffectsRepository::NyquistEffectsRepository(const muse::modularity::ContextPtr& ctx)
    : muse::Contextable(ctx), m_helper{m_module, muse::audio::AudioResourceType::NyquistPlugin}
{
}

void au::effects::NyquistEffectsRepository::registerSpectralEffects() const
{
    for (const auto& meta : effectMetaList()) {
        if (meta.category == utils::effectCategoryToString(EffectCategory::SpectralTools)) {
            const NyquistBase* const nyquistEffect
                = dynamic_cast<const NyquistBase*>(EffectManager::Get().GetEffect(au3::wxFromString(meta.id)));
            if (!nyquistEffect) {
                continue;
            }
            std::optional<spectrogram::SpectralEffectId> spectralEffectIdOpt;
            const auto spectralEffectId = nyquistEffect->GetSpectralEffectId();
            if (spectralEffectId == "DeleteSelection") {
                spectralEffectIdOpt = spectrogram::SpectralEffectId::DeleteSelection;
            } else if (spectralEffectId == "DeleteCenterFrequency") {
                spectralEffectIdOpt = spectrogram::SpectralEffectId::DeleteCenterFrequency;
            } else if (spectralEffectId == "AmplifySelection") {
                spectralEffectIdOpt = spectrogram::SpectralEffectId::AmplifySelection;
            } else if (spectralEffectId == "AmplifyCenterFrequency") {
                spectralEffectIdOpt = spectrogram::SpectralEffectId::AmplifyCenterFrequency;
            }
            if (spectralEffectIdOpt) {
                spectrogram::SpectralEffect spectralEffect;
                spectralEffect.spectralEffectId = *spectralEffectIdOpt;
                spectralEffect.action = effects::makeEffectAction(effects::EFFECT_OPEN_ACTION, meta.id);
                spectralEffect.title = meta.title;
                spectralEffectsRegister()->registerSpectralEffect(spectralEffect);
            }
        }
    }
}

au::effects::EffectMetaList au::effects::NyquistEffectsRepository::effectMetaList() const
{
    return m_helper.effectMetaList();
}

bool au::effects::NyquistEffectsRepository::ensurePluginIsLoaded(const EffectId& effectId) const
{
    return m_helper.ensurePluginIsLoaded(effectId);
}
