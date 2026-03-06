/*
* Audacity: A Digital Audio Editor
*/
#include "nyquisteffectsrepository.h"

#include "effects/effects_base/internal/au3/au3effectsutils.h"
#include "effects/effects_base/internal/effectsutils.h"
#include "effects/effects_base/effectstypes.h"
#include "au3wrap/internal/wxtypes_convert.h"

#include "au3-module-manager/PluginDescriptor.h"
#include "au3-module-manager/PluginManager.h"
#include "au3-effects/EffectManager.h"
#include "spectrogram/spectrogramtypes.h"

au::effects::NyquistEffectsRepository::NyquistEffectsRepository(const muse::modularity::ContextPtr& ctx,
                                                                std::unique_ptr<muse::audioplugins::IAudioPluginsScanner> nyquistPluginScanner,
                                                                std::shared_ptr<muse::audioplugins::IAudioPluginMetaReader> nyquistPluginMetaReader)
    : muse::Injectable(ctx), m_nyquistPluginScanner{std::move(nyquistPluginScanner)}, m_nyquistPluginMetaReader{std::move(
                                                                                                                    nyquistPluginMetaReader)}
{
}

void au::effects::NyquistEffectsRepository::init()
{
    for (const auto& meta : effectMetaList()) {
        if (meta.category == utils::builtinEffectCategoryIdString(BuiltinEffectCategoryId::SpectralTools)) {
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
                spectralEffectsRegister()->registerSpectralEffect(spectralEffect);
            }
        }
    }
}

au::effects::EffectMetaList au::effects::NyquistEffectsRepository::effectMetaList() const
{
    au::effects::EffectMetaList effects;

    for (const muse::io::path_t& path : m_nyquistPluginScanner->scanPlugins()) {
        const muse::RetVal<muse::audio::AudioResourceMetaList> list = m_nyquistPluginMetaReader->readMeta(path);
        if (list.ret) {
            for (const muse::audio::AudioResourceMeta& resourceMeta : list.val) {
                // Get the AU3 descriptor to access the effect type and proper name
                const auto ptr = PluginManager::Get().GetPlugin(au3::wxFromString(muse::String::fromStdString(resourceMeta.id)));
                if (!ptr) {
                    continue;
                }

                const PluginDescriptor& desc = *ptr;

                au::effects::EffectMeta meta;
                meta.id = muse::String(resourceMeta.id.c_str());
                meta.family = au::effects::EffectFamily::Nyquist;
                meta.category = utils::builtinEffectCategoryIdString(toAu4EffectCategory(desc.GetEffectGroup()));
                meta.title = muse::String::fromStdString(desc.GetSymbol().Msgid().Translation().ToStdString());
                meta.isRealtimeCapable = desc.IsEffectRealtime();
                meta.vendor = muse::String::fromStdString(resourceMeta.vendor);
                meta.path = path;

                // Map the AU3 effect type to AU4 effect type for proper menu placement
                // Type mapping for Nyquist effects (based on $type directive in .ny files):
                // - $type process  → Effects menu (Processor)
                // - $type generate → Generate menu (Generator)
                // - $type analyze  → Analyze menu (Analyzer)
                // - $type tool     → Tools menu (Tool)
                switch (desc.GetEffectType()) {
                case ::EffectTypeGenerate:
                    meta.type = au::effects::EffectType::Generator;
                    break;
                case ::EffectTypeProcess:
                    meta.type = au::effects::EffectType::Processor;
                    break;
                case ::EffectTypeAnalyze:
                    meta.type = au::effects::EffectType::Analyzer;
                    break;
                case ::EffectTypeTool:
                    meta.type = au::effects::EffectType::Tool;
                    break;
                default:
                    meta.type = au::effects::EffectType::Unknown;
                }

                effects.push_back(std::move(meta));
            }
        } else {
            LOGW() << "Failed to read plugin meta for " << path << ": " << list.ret.toString();
            continue;
        }
    }

    return effects;
}

bool au::effects::NyquistEffectsRepository::ensurePluginIsLoaded(const EffectId& effectId) const
{
    if (!PluginManager::Get().IsPluginLoaded(au3::wxFromString(effectId))) {
        return PluginManager::Get().Load(au3::wxFromString(effectId)) != nullptr;
    }
    return true;
}
