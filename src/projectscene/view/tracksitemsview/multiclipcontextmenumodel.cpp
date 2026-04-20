/**********************************************************************

  Audacity: A Digital Audio Editor

**********************************************************************/

#include "multiclipcontextmenumodel.h"

using namespace au::projectscene;
using namespace muse;
using namespace muse::uicomponents;
using namespace muse::actions;

void MultiClipContextMenuModel::load()
{
    AbstractMenuModel::load();

    MenuItemList items = makeItems();
    setItems(items);
}

MenuItemList MultiClipContextMenuModel::makeItems()
{
    MenuItemList cutAndItems {
        makeMenuItem("cut-leave-gap",
                     muse::TranslatableString("multiclip", "Cut and leave gap")),
        makeMenuItem("cut-per-track-ripple",
                     muse::TranslatableString("multiclip", "Cut and close gap on this track")),
        makeMenuItem("cut-all-tracks-ripple",
                     muse::TranslatableString("multiclip", "Cut and close gap on all tracks")),
    };

    MenuItemList deleteAndItems {
        makeMenuItem("delete-leave-gap",
                     muse::TranslatableString("multiclip", "Delete and leave gap")),
        makeMenuItem("delete-per-track-ripple",
                     muse::TranslatableString("multiclip", "Delete and close gap on this track")),
        makeMenuItem("delete-all-tracks-ripple",
                     muse::TranslatableString("multiclip", "Delete and close gap on all tracks")),
    };

    MenuItemList items {
        makeMenuItem("action://trackedit/cut"),
        makeMenuItem("action://trackedit/copy"),
        makeMenuItem("action://trackedit/delete"),
        makeSeparator(),
        makeMenu(muse::TranslatableString("multiclip", "Cut and…"), cutAndItems, "menu-cut-and"),
        makeMenu(muse::TranslatableString("multiclip", "Delete and…"), deleteAndItems, "menu-delete-and"),
        makeSeparator(),
        makeMenuItem("group-clips"),
        makeMenuItem("ungroup-clips"),
    };

    return items;
}
