/*
* Audacity: A Digital Audio Editor
*/
#include "effectsprovider.h"

#include "log.h"

using namespace au::effects;

static muse::String VST_CATEGORY_ID = u"vst";

static muse::String categoryId(muse::audio::AudioResourceType type)
{
    switch (type) {
    case muse::audio::AudioResourceType::VstPlugin: return VST_CATEGORY_ID;
    default: break;
    }

    return u"";
}

void EffectsProvider::reloadEffects()
{
    m_effects.clear();
    m_effectsCategories.clear();

    std::vector<muse::audioplugins::AudioPluginInfo> allEffects = knownPlugins()->pluginInfoList();

    for (const muse::audioplugins::AudioPluginInfo& info : allEffects) {
        EffectMeta meta;
        meta.id = muse::String(info.meta.id.c_str());
        meta.title = muse::String(info.meta.id.c_str());

        meta.categoryId = categoryId(info.meta.type);

        m_effects.push_back(meta);
    }

    m_effectsChanged.notify();
}

EffectMetaList EffectsProvider::effectMetaList() const
{
    return m_effects;
}

muse::async::Notification EffectsProvider::effectMetaListChanged() const
{
    return m_effectsChanged;
}

EffectCategoryList EffectsProvider::effectsCategoryList() const
{
    return {
#ifdef AU_MODULE_VST
        { VST_CATEGORY_ID, u"VST" }
#endif
    };
}

EffectMeta EffectsProvider::meta(const muse::String& id) const
{
    for (const EffectMeta& meta : m_effects) {
        if (meta.id == id) {
            return meta;
        }
    }

    LOGE() << "not found meta: " << id;
    return EffectMeta();
}
