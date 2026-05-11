/*
* Audacity: A Digital Audio Editor
*/
#include "knownaudiopluginsconfigurator.h"

#include "global/serialization/json.h"

#include "effects/builtin/internal/builtintypes.h"

using namespace au::effects;

namespace {
// resource type builtin effects were cached under before the rename to
// builtin::AUDIO_RESOURCE_TYPE_NAME
constexpr std::string_view LEGACY_BUILTIN_TYPE_NAME = "NativeEffect";
}

void KnownAudioPluginsConfigurator::init()
{
    if (!m_migrations()) {
        return;
    }

    // v2 -> v3: legacy caches stored builtin effects as "NativeEffect", which
    // no longer maps to any family and would trip the asserts in effectsutils;
    // rename it to the current builtin type.
    m_migrations()->registerMigration(2, [](const muse::JsonArray& plugins) {
        muse::JsonArray out;
        for (size_t i = 0; i < plugins.size(); ++i) {
            muse::JsonObject obj = plugins.at(i).toObject();
            muse::JsonObject meta = obj.value("meta").toObject();
            if (meta.value("type").toStdString() == LEGACY_BUILTIN_TYPE_NAME) {
                meta.set("type", std::string(builtin::AUDIO_RESOURCE_TYPE_NAME));
                obj.set("meta", meta);
            }
            out << obj;
        }
        return out;
    });
}
