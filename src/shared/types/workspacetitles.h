/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <string>

#include "framework/actions/actiontypes.h"
#include "framework/global/types/translatablestring.h"
#include "framework/ui/uiaction.h"
#include "framework/uicomponents/qml/Muse/UiComponents/menuitem.h"

namespace au::shared {
//! NOTE Give the workspaces we ship a translated title;
//! workspaces created by the user keep their own name.
inline muse::TranslatableString workspaceTitle(const std::string& name)
{
    if (name == "Classic") {
        //: The name of a built-in workspace (an interface layout);
        //: an adjective in languages where that applies
        return muse::TranslatableString("workspace", "Classic");
    } else if (name == "Modern") {
        //: The name of a built-in workspace (an interface layout);
        //: an adjective in languages where that applies
        return muse::TranslatableString("workspace", "Modern");
    } else if (name == "Music") {
        //: The name of a built-in workspace (an interface layout);
        //: an adjective in languages where that applies
        return muse::TranslatableString("workspace", "Music");
    }

    return muse::TranslatableString::untranslatable(muse::String::fromStdString(name));
}

inline const muse::uicomponents::MenuItemList& translateWorkspaceTitles(const muse::uicomponents::MenuItemList& items)
{
    for (muse::uicomponents::MenuItem* item : items) {
        const muse::actions::ActionData args = item->args();
        if (args.empty()) {
            continue;
        }

        muse::ui::UiAction action = item->action();
        action.title = workspaceTitle(args.arg<std::string>(0));
        item->setAction(action);
    }

    return items;
}
}
