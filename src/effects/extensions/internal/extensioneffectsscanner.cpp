/*
 * Audacity: A Digital Audio Editor
 */
#include "extensioneffectsscanner.h"

#include <algorithm>
#include <utility>

#include "framework/audioplugins/iregisteraudiopluginsscenario.h"
#include "framework/extensions/iextensionsprovider.h"
#include "framework/global/log.h"

#include "extensioneffectloader.h"
#include "extensioneffectsrepository.h"
#include "extensiontypes.h"

namespace au::effects::extensions {
ExtensionEffectsScanner::ExtensionEffectsScanner(std::shared_ptr<muse::extensions::IExtensionsProvider> extensionsProvider,
                                                 std::shared_ptr<ExtensionEffectsRepository> repository,
                                                 std::shared_ptr<ExtensionEffectLoader> effectLoader)
    : m_extensionsProvider(std::move(extensionsProvider)), m_repository(std::move(repository)),
    m_effectLoader(std::move(effectLoader))
{
}

muse::io::paths_t ExtensionEffectsScanner::scanPlugins(muse::Progress*) const
{
    m_extensionsProvider->reloadExtensions();
    return updateRepository();
}

muse::io::paths_t ExtensionEffectsScanner::updateRepository() const
{
    if (m_repository->reload(m_extensionsProvider->manifestList(muse::extensions::Filter::Enabled))) {
        m_effectLoader->retireAll();
    }
    return m_repository->pluginPaths();
}

void ExtensionEffectsScanner::refreshPlugins(
    muse::audioplugins::IRegisterAudioPluginsScenario& registerAudioPluginsScenario) const
{
    const muse::io::paths_t paths = updateRepository();
    for (const auto& path : paths) {
        if (!knownPlugins()->exists(path)) {
            const muse::Ret result = registerAudioPluginsScenario.registerPlugin(path);
            if (!result) {
                LOGE() << "failed to register extension effects: " << path << ", error: " << result.toString();
            }
        }
    }
    for (const auto& plugin : knownPlugins()->pluginInfoList()) {
        if (plugin.meta.type == AUDIO_RESOURCE_TYPE_NAME
            && std::find(paths.cbegin(), paths.cend(), plugin.path) == paths.cend()) {
            const muse::Ret result = knownPlugins()->removePluginsAtPath(plugin.path);
            if (!result) {
                LOGE() << "failed to remove obsolete extension effects: " << plugin.path << ", error: " << result.toString();
            }
        }
    }
    const muse::Ret result = knownPlugins()->load();
    if (!result) {
        LOGE() << "failed to reload the audio plugin registry: " << result.toString();
    }
}
} // namespace au::effects::extensions
