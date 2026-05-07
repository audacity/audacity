/*
 * Audacity: A Digital Audio Editor
 */
#include "effectsviewutils.h"

namespace au::effects {
muse::uicomponents::MenuItemList utils::toMenuItemList(const std::vector<DropdownOption>& options, int selectedIndex, QObject* parent)
{
    muse::uicomponents::MenuItemList result;
    for (auto i = 0; i < static_cast<int>(options.size()); ++i) {
        auto item = new muse::uicomponents::MenuItem(parent);
        item->setId(options[i].id);
        item->setTitle(muse::TranslatableString::untranslatable({ options[i].title }));
        item->setSelectable(true);
        item->setSelected(i == selectedIndex);
        auto state = item->state();
        state.enabled = true;
        item->setState(state);
        result.append(item);
    }
    return result;
}
}
