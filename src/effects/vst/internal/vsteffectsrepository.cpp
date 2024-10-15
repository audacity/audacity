/*
* Audacity: A Digital Audio Editor
*/
#include "vsteffectsrepository.h"

using namespace au::effects;

EffectMetaList VstEffectsRepository::effectMetaList() const
{
    EffectMetaList effects;

    //! TODO Add filter by VST
    std::vector<muse::audioplugins::AudioPluginInfo> allEffects = knownPlugins()->pluginInfoList();

    for (const muse::audioplugins::AudioPluginInfo& info : allEffects) {
        EffectMeta meta;
        meta.id = muse::String(info.meta.id.c_str());
        meta.title = muse::String(info.meta.id.c_str());

        meta.categoryId = VST_CATEGORY_ID;

        effects.push_back(std::move(meta));
    }

    return effects;
}
