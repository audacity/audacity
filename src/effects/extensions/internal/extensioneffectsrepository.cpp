/*
 * Audacity: A Digital Audio Editor
 */
#include "extensioneffectsrepository.h"

#include <algorithm>
#include <optional>
#include <set>

#include "effects/effects_base/internal/effectsutils.h"
#include "framework/global/containers.h"
#include "framework/extensions/extensionbundle.h"

#include "extensioneffect.h"

namespace au::effects::extensions {
namespace {
std::string stringMember(const muse::ValMap& object, const std::string& name)
{
    const muse::Val value = muse::value(object, name);
    return value.type() == muse::Val::Type::String ? value.toString() : std::string{};
}

std::optional<EffectDescriptor> effectDescriptor(const muse::extensions::Manifest& manifest, const muse::ValMap& object)
{
    EffectDescriptor descriptor;
    descriptor.extensionId = manifest.uri.toString();
    descriptor.effectId = stringMember(object, "id");
    descriptor.title = stringMember(object, "title");
    descriptor.description = stringMember(object, "description");
    descriptor.vendor = stringMember(object, "vendor");
    descriptor.version = manifest.version;
    descriptor.group = stringMember(object, "group");
    descriptor.factory = stringMember(object, "factory");
    if (descriptor.factory.empty()) {
        descriptor.factory = "createEffect";
    }
    descriptor.manifest = manifest;

    if (descriptor.effectId.empty() || descriptor.title.empty()
        || (descriptor.group != "generate" && descriptor.group != "effect" && descriptor.group != "analyze"
            && descriptor.group != "tools")) {
        return std::nullopt;
    }

    const muse::io::path_t script = stringMember(object, "script");
    if (!muse::extensions::resolveBundleFile(muse::io::dirpath(manifest.path), script)) {
        return std::nullopt;
    }
    descriptor.scriptPath = script;
    return descriptor;
}

bool sameEntry(const ExtensionEffectEntry& a, const ExtensionEffectEntry& b)
{
    const EffectDescriptor& x = a.descriptor;
    const EffectDescriptor& y = b.descriptor;
    return a.id == b.id && a.bundlePath == b.bundlePath && x.extensionId == y.extensionId && x.effectId == y.effectId
           && x.title == y.title && x.description == y.description && x.vendor == y.vendor && x.version == y.version
           && x.group == y.group && x.factory == y.factory && x.scriptPath == y.scriptPath;
}
} // namespace

void ExtensionEffectsRepository::initialize(const muse::extensions::ManifestList& manifests)
{
    if (!m_initialized) {
        reload(manifests);
    }
}

bool ExtensionEffectsRepository::reload(const muse::extensions::ManifestList& manifests)
{
    std::vector<ExtensionEffectEntry> effects;
    for (const auto& manifest : manifests) {
        const auto found = manifest.contributes.find("audacity.effects");
        if (found == manifest.contributes.end()) {
            continue;
        }
        for (const muse::ValMap& item : found->second) {
            auto descriptor = effectDescriptor(manifest, item);
            if (!descriptor) {
                continue;
            }
            ExtensionEffect effect(*descriptor);
            effects.push_back({
                    utils::effectId(&effect),
                    std::move(*descriptor),
                    manifest.path,
                });
        }
    }

    const bool changed = !m_initialized || effects.size() != m_effects.size() || !std::equal(effects.begin(), effects.end(),
                                                                                             m_effects.begin(), sameEntry);
    m_initialized = true;
    m_effects = std::move(effects);
    return changed;
}

const std::vector<ExtensionEffectEntry>& ExtensionEffectsRepository::effects() const
{
    return m_effects;
}

const ExtensionEffectEntry* ExtensionEffectsRepository::effect(const EffectId& id) const
{
    const auto found = std::find_if(m_effects.begin(), m_effects.end(), [&](const auto& entry) {
        return entry.id == id;
    });
    return found == m_effects.end() ? nullptr : &*found;
}

muse::io::paths_t ExtensionEffectsRepository::pluginPaths() const
{
    std::set<muse::io::path_t> paths;
    for (const auto& effect : m_effects) {
        paths.insert(effect.bundlePath);
    }
    return { paths.begin(), paths.end() };
}

bool ExtensionEffectsRepository::contains(const muse::io::path_t& path) const
{
    return std::any_of(m_effects.begin(), m_effects.end(), [&](const auto& effect) {
        return effect.bundlePath == path;
    });
}
} // namespace au::effects::extensions
