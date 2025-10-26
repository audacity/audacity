/*
 * Audacity: A Digital Audio Editor
 */

#pragma once

// This is required because this file is now used in effects/builtin
#include "effects/effects_base/effectstypes.h"
#include "uicomponents/view/menuitem.h"

namespace au::effects::utils {
using EffectFilter = std::function<bool (const EffectMeta&)>;

muse::uicomponents::MenuItemList destructiveEffectMenu(EffectMenuOrganization organization, EffectMetaList metaList,
                                                       const EffectFilter& filter, IEffectMenuItemFactory& effectMenu);
muse::uicomponents::MenuItemList realtimeEffectMenu(EffectMenuOrganization organization, EffectMetaList metaList,
                                                    const EffectFilter& filter, IEffectMenuItemFactory& effectMenu);
muse::String builtinEffectCategoryIdString(BuiltinEffectCategoryId category);
int builtinEffectCategoryIdOrder(const muse::String& category);
}
