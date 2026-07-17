/*
 * Audacity: A Digital Audio Editor
 */
#include "missingeffectchecker.h"

#include "framework/global/log.h"
#include "framework/global/types/uri.h"
#include "framework/global/types/val.h"

#include "au3-realtime-effects/RealtimeEffectState.h"
#include "effectsutils.h"

#include <set>

namespace au::effects {
void MissingEffectChecker::warnIfEffectsMissing()
{
    const auto project = globalContext()->currentTrackeditProject();
    IF_ASSERT_FAILED(project && "call expected on opened project") {
        return;
    }

    std::set<EffectId> missingEffectIds;

    const std::vector<trackedit::TrackId> trackIds = project->trackIdList();
    for (const auto trackId : trackIds) {
        const auto effectStack = realtimeEffectService()->effectStack(trackId);
        if (!effectStack) {
            continue;
        }
        for (const auto& state : *effectStack) {
            if (!realtimeEffectService()->isAvailable(state)) {
                missingEffectIds.insert(EffectId::fromStdString(state->GetID().ToStdString()));
            }
        }
    }

    if (missingEffectIds.empty()) {
        return;
    }

    // prefer cached metadata, fall back to parsing the id (which carries no version)
    muse::ValList missingPluginInfos;
    missingPluginInfos.reserve(missingEffectIds.size());
    for (const auto& id : missingEffectIds) {
        const EffectMeta cached = effectsProvider()->meta(id);
        const bool hasRichMeta = cached.isValid() && !cached.title.empty();

        const std::string name = hasRichMeta
                                 ? cached.title.toStdString()
                                 : utils::parseEffectName(id);
        const std::string vendor = hasRichMeta
                                   ? cached.vendor.toStdString()
                                   : utils::parseEffectVendor(id);
        const std::string path = hasRichMeta
                                 ? cached.path.toStdString()
                                 : utils::parseEffectPath(id);

        missingPluginInfos.push_back(muse::Val { muse::ValMap {
                                                     { "name", muse::Val(name) },
                                                     { "vendor", muse::Val(vendor) },
                                                     { "path", muse::Val(path) }
                                                 } });
    }

    muse::UriQuery query("audacity://effects/missing_plugins");
    query.addParam("missingPlugins", muse::Val(missingPluginInfos));
    interactive()->openSync(query);
}
}
