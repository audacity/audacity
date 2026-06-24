/*
* Audacity: A Digital Audio Editor
*/
#include "knownaudiopluginsconfigurator.h"

#include "global/serialization/json.h"

#include "effects/builtin/internal/builtintypes.h"

using namespace au::effects;

namespace {
// Pre-revamp builtin effects were cached under this resource type; the
// audioplugins revamp renamed it to builtin::AUDIO_RESOURCE_TYPE_NAME.
constexpr std::string_view LEGACY_BUILTIN_TYPE_NAME = "NativeEffect";
}

void KnownAudioPluginsConfigurator::init()
{
    if (!m_migrations()) {
        return;
    }

    // v2 -> v3 is MuseScore's hasNativeEditorSupport slot; Audacity reuses it
    // for its own work. MuseScore's hasNativeEditorSupport concept never
    // applied here, but pre-revamp caches stored builtin effects with the
    // "NativeEffect" resource type, which no longer maps to any family (it
    // would resolve to EffectFamily::Unknown and trip the asserts in
    // effectsutils). Rename it to the current builtin type so every legacy
    // cache reaches v3 with a recognised type. All older versions funnel
    // through this final step, so it covers v0/v1/v2 caches alike.
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

    // v3 -> v4: nyquist effects bundled with the app, do not store full path in the configuration file
    const std::shared_ptr<muse::IGlobalConfiguration> globalConfiguration = m_globalConfiguration();
    m_migrations()->registerMigration(3, [globalConfiguration](const muse::JsonArray& plugins) {
        muse::JsonArray out;
        for (size_t i = 0; i < plugins.size(); ++i) {
            muse::JsonObject obj = plugins.at(i).toObject();

            const std::string path = obj.value("path").toStdString();
            if (globalConfiguration->isBundledWithApp(path)) {
                const std::string portable = globalConfiguration->toBundledPath(path).toStdString();
                obj.set("path", portable);

                muse::JsonObject meta = obj.value("meta").toObject();
                std::string id = meta.value("id").toStdString();
                if (id.size() >= path.size() && id.compare(id.size() - path.size(), path.size(), path) == 0) {
                    meta.set("id", id.substr(0, id.size() - path.size()) + portable);
                    obj.set("meta", meta);
                }
            }

            out << obj;
        }
        return out;
    });
}
