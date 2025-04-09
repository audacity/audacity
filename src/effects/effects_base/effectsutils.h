/*
 * Audacity: A Digital Audio Editor
 */

#pragma once

#include "effectstypes.h"
#include "uicomponents/view/menuitem.h"

namespace au::effects::utils {
using EffectFilter = std::function<bool (const EffectMeta&)>;

muse::uicomponents::MenuItemList effectMenus(EffectMenuOrganization organization, EffectMetaList metaList, const EffectFilter& filter,
                                             IEffectMenuItemFactory& effectMenu);
}
