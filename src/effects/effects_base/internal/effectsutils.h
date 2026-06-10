/*
 * Audacity: A Digital Audio Editor
 */

#pragma once

// This is required because this file is now used in effects/builtin
#include "effects/effects_base/effectstypes.h"

#include "framework/uicomponents/qml/Muse/UiComponents/menuitem.h"
#include "framework/audio/common/audiotypes.h"

class EffectDefinitionInterface;

namespace au::effects::utils {
using EffectFilter = std::function<bool (const EffectMeta&)>;

muse::uicomponents::MenuItemList destructiveEffectMenu(EffectMenuOrganization organization, EffectMetaList metaList,
                                                       const EffectFilter& filter, IEffectMenuItemFactory& effectMenu);

muse::uicomponents::MenuItemList realtimeEffectMenu(EffectMenuOrganization organization, EffectMetaList metaList,
                                                    const EffectFilter& filter, IEffectMenuItemFactory& effectMenu);

EffectId effectId(const EffectDefinitionInterface* effect);

bool isEffectId(const EffectId& effectId);

std::string parseEffectName(const EffectId& effectId);
std::string parseEffectVendor(const EffectId& effectId);
std::string parseEffectFamily(const EffectId& effectId);
std::string parseEffectPath(const EffectId& effectId);

muse::String effectTypeToString(EffectType type);
EffectType effectTypeFromString(const muse::String& type);

muse::String effectCategoryToString(EffectCategory category);
EffectCategory effectCategoryFromString(const muse::String& category);

muse::String effectFamilyToString(EffectFamily family);
EffectFamily effectFamilyFromString(const muse::String& family);

// Persistence identifier for a plugin family, as stored in the muse_framework
// plugin cache (PluginMeta::type). Distinct from effectFamilyToString above,
// which returns a human-readable display label. Each family owns its identifier
// constant (AUDIO_RESOURCE_TYPE_NAME in its <family>types.h); to add a family,
// define that constant and add a case in the .cpp.
std::string effectFamilyToCacheType(EffectFamily family);
EffectFamily effectFamilyFromCacheType(const std::string& cacheType);

// True if the cached plugin meta belongs to the given family.
bool isFamilyType(const muse::audioplugins::PluginMeta& meta, EffectFamily family);

muse::audioplugins::PluginMeta auToMuseEffectMeta(const EffectMeta& meta);
EffectMeta museToAuEffectMeta(const muse::io::path_t& path, const muse::audioplugins::PluginMeta& meta,
                              muse::audioplugins::AudioPluginState state = muse::audioplugins::AudioPluginState::Validated);
}
