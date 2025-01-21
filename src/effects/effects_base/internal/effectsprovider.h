/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "async/asyncable.h"
#include "modularity/ioc.h"
#include "global/iinteractive.h"
#include "context/iglobalcontext.h"
#include "playback/iplayback.h"
#include "effects/builtin/ibuiltineffectsrepository.h"
#include "effects/vst/ivsteffectsrepository.h"
#include "effects/nyquist/inyquisteffectsrepository.h"
#include "../ieffectsconfiguration.h"
#include "../ieffectviewlaunchregister.h"

#include "../ieffectsprovider.h"

class EffectBase;
class EffectSettingsAccess;
namespace au::effects {
class EffectsProvider : public IEffectsProvider, public muse::async::Asyncable
{
    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<IEffectsConfiguration> configuration;
    muse::Inject<IBuiltinEffectsRepository> builtinEffectsRepository;
    muse::Inject<IVstEffectsRepository> vstEffectsRepository;
    muse::Inject<INyquistEffectsRepository> nyquistEffectsRepository;
    muse::Inject<muse::IInteractive> interactive;
    muse::Inject<playback::IPlayback> playback;
    muse::Inject<IEffectViewLaunchRegister> viewLaunchRegister;

public:
    void reloadEffects();

    EffectMetaList effectMetaList() const override;
    muse::async::Notification effectMetaListChanged() const override;

    EffectCategoryList effectsCategoryList() const override;

    EffectMeta meta(const EffectId& effectId) const override;
    std::string effectName(const std::string& effectId) const override;
    std::string effectName(const effects::RealtimeEffectState& state) const override;
    std::string effectSymbol(const std::string& effectId) const override;
    Effect* effect(const EffectId& effectId) const override;

    bool supportsMultipleClipSelection(const EffectId& effectId) const override;

    muse::Ret showEffect(const EffectId& effectId, const EffectInstanceId& instanceId) override;

    void showEffect(const RealtimeEffectStatePtr& state) const override;
    void hideEffect(const RealtimeEffectStatePtr& state) const override;
    void toggleShowEffect(const RealtimeEffectStatePtr& state) const override;

    muse::Ret performEffect(au3::Au3Project& project, Effect* effect, std::shared_ptr<EffectInstance> effectInstance,
                            EffectSettings& settings) override;

    muse::Ret previewEffect(au3::Au3Project& project, Effect* effect, EffectSettings& settings) override;

private:

    bool isVstSupported() const;
    bool isNyquistSupported() const;

    muse::Ret doEffectPreview(EffectBase& effect, EffectSettings& settings);

    mutable EffectMetaList m_effects;
    muse::async::Notification m_effectsChanged;

    mutable EffectCategoryList m_effectsCategories;
};
}
