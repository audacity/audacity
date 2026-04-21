/**********************************************************************

  Audacity: A Digital Audio Editor

**********************************************************************/

#include "selectioncontextmenumodel.h"

using namespace au::projectscene;
using namespace muse;
using namespace muse::uicomponents;
using namespace muse::actions;

void SelectionContextMenuModel::load()
{
    AbstractMenuModel::load();

    MenuItemList items = makeItems();
    setItems(items);
}

MenuItemList SelectionContextMenuModel::makeItems()
{
    MenuItemList cutAndItems {
        makeMenuItem("cut-leave-gap",
                     muse::TranslatableString("selection", "Cut and leave gap")),
        makeMenuItem("cut-per-clip-ripple",
                     muse::TranslatableString("selection", "Cut and close gap")),
        makeMenuItem("cut-per-track-ripple",
                     muse::TranslatableString("selection", "Cut and close gap on this track")),
        makeMenuItem("cut-all-tracks-ripple",
                     muse::TranslatableString("selection", "Cut and close gap on all tracks")),
    };

    MenuItemList pasteAndItems {
        makeMenuItem("action://trackedit/paste-overlap",
                     muse::TranslatableString("selection", "Paste and overlap")),
        makeMenuItem("action://trackedit/paste-insert",
                     muse::TranslatableString("selection", "Paste and make room on this track")),
        makeMenuItem("action://trackedit/paste-insert-all-tracks-ripple",
                     muse::TranslatableString("selection", "Paste and make room on all tracks")),
    };

    MenuItemList deleteAndItems {
        makeMenuItem("delete-leave-gap",
                     muse::TranslatableString("selection", "Delete and leave gap")),
        makeMenuItem("delete-per-clip-ripple",
                     muse::TranslatableString("selection", "Delete and close gap")),
        makeMenuItem("delete-per-track-ripple",
                     muse::TranslatableString("selection", "Delete and close gap on this track")),
        makeMenuItem("delete-all-tracks-ripple",
                     muse::TranslatableString("selection", "Delete and close gap on all tracks")),
    };

    MenuItemList items {
        makeMenuItem("action://trackedit/cut"),
        makeMenuItem("action://trackedit/copy"),
        makeMenuItem("action://trackedit/paste-default"),
        makeMenuItem("duplicate"),
        makeMenuItem("action://trackedit/delete"),
        makeSeparator(),
        makeMenu(muse::TranslatableString("selection", "Cut and…"), cutAndItems, "menu-cut-and"),
        makeMenu(muse::TranslatableString("selection", "Paste and…"), pasteAndItems, "menu-paste-and"),
        makeMenu(muse::TranslatableString("selection", "Delete and…"), deleteAndItems, "menu-delete-and"),
        makeSeparator(),
        makeMenuItem("split")
    };

    return items;
}
