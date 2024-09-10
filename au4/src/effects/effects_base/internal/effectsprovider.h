/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"
#include "global/iinteractive.h"
#include "effects/builtin/ibuiltineffectsrepository.h"
#include "effects/vst/ivsteffectsrepository.h"
#include "effects/nyquist/inyquisteffectsrepository.h"
#include "../ieffectsconfiguration.h"

#include "../ieffectsprovider.h"

namespace au::effects {
class EffectsProvider : public IEffectsProvider
{
    muse::Inject<IEffectsConfiguration> configuration;
    muse::Inject<IBuiltinEffectsRepository> builtinEffectsRepository;
    muse::Inject<IVstEffectsRepository> vstEffectsRepository;
    muse::Inject<INyquistEffectsRepository> nyquistEffectsRepository;
    muse::Inject<muse::IInteractive> interactive;

public:
    void reloadEffects() override;

    EffectMetaList effectMetaList() const override;
    muse::async::Notification effectMetaListChanged() const override;

    EffectCategoryList effectsCategoryList() const override;

    EffectMeta meta(const muse::String& effectId) const override;

    muse::Ret showEffect(const muse::String& type, const EffectInstanceId& instanceId) override;

    muse::Ret performEffect(AudacityProject& project, Effect* effect, EffectSettings& settings,
                            const EffectTimeParams& timeParams) override;

private:

    bool isVstSupported() const;
    bool isNyquistSupported() const;

    mutable EffectMetaList m_effects;
    muse::async::Notification m_effectsChanged;

    mutable EffectCategoryList m_effectsCategories;
};
}
