/*
* Audacity: A Digital Audio Editor
*/
#include "effectsprovider.h"

#include "global/translation.h"

#include "../builtin/builtineffects.h"

#include "log.h"

using namespace muse;
using namespace au::effects;

static const char16_t* VIEWER_URI = u"audacity://effects/viewer?type=%1&instanceId=%2";

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

    // build-in
    {
        BuiltinEffects loader;
        EffectMetaList metaList = loader.effectMetaList();
        for (EffectMeta meta : metaList) {
            m_effects.push_back(std::move(meta));
        }
    }

    // plugins
    {
        std::vector<muse::audioplugins::AudioPluginInfo> allEffects = knownPlugins()->pluginInfoList();

        for (const muse::audioplugins::AudioPluginInfo& info : allEffects) {
            EffectMeta meta;
            meta.id = muse::String(info.meta.id.c_str());
            meta.title = muse::String(info.meta.id.c_str());

            meta.categoryId = categoryId(info.meta.type);

            m_effects.push_back(std::move(meta));
        }
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
    static const EffectCategoryList list = {
        { BUILTIN_CATEGORY_ID, muse::mtrc("effects", "Build-in") },
#ifdef AU_MODULE_VST
        {
            VST_CATEGORY_ID, u"VST"
        }
#endif
    };

    return list;
}

EffectMeta EffectsProvider::meta(const muse::String& effectId) const
{
    for (const EffectMeta& meta : m_effects) {
        if (meta.id == effectId) {
            return meta;
        }
    }

    LOGE() << "not found meta: " << effectId;
    return EffectMeta();
}

muse::Ret EffectsProvider::showEffect(const muse::String& type, const EffectInstanceId& instanceId)
{
    LOGD() << "try open effect: " << type << ", instanceId: " << instanceId;

    RetVal<Val> rv = interactive()->open(String(VIEWER_URI).arg(type).arg(instanceId).toStdString());

    LOGD() << "open ret: " << rv.ret.toString();

    return rv.ret;
}
