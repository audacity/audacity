/*
 * Audacity: A Digital Audio Editor
 */
#include "builtineffectsmetareader.h"

#include "effects/effects_base/internal/effectsutils.h"

namespace au::effects {
muse::audio::AudioResourceType BuiltinEffectsMetaReader::metaType() const
{
    return muse::audio::AudioResourceType::NativeEffect;
}

bool BuiltinEffectsMetaReader::canReadMeta(const muse::io::path_t& pluginPath) const
{
    const auto metas = builtinEffectsRepository()->effectMetaList();
    return std::any_of(metas.begin(), metas.end(), [&pluginPath](const EffectMeta& meta) { return meta.path == pluginPath; });
}

muse::RetVal<muse::audio::AudioResourceMetaList> BuiltinEffectsMetaReader::readMeta(const muse::io::path_t& pluginPath) const
{
    using namespace muse;
    const auto metas = builtinEffectsRepository()->effectMetaList();
    const auto it = std::find_if(metas.begin(), metas.end(), [&pluginPath](const EffectMeta& meta) { return meta.path == pluginPath; });

    if (it == metas.end()) {
        return RetVal<audio::AudioResourceMetaList>::make_ret(Ret::Code::BadData,
                                                              "No builtin effect found with path: " + pluginPath.toStdString());
    }

    audio::AudioResourceMeta meta = utils::auToMuseEffectMeta(*it);
    return RetVal<audio::AudioResourceMetaList>::make_ok({ meta });
}
}
