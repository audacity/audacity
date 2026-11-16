/**********************************************************************

  Audacity: A Digital Audio Editor

**********************************************************************/

#include "multiclipcontextmenumodel.h"

#include "trackedit/trackeditcommands.h"

using namespace au::projectscene;
using namespace muse;
using namespace muse::uicomponents;

void MultiClipContextMenuModel::load()
{
    AbstractMenuModel::load();

    MenuItemList items = makeItems();
    setItems(items);
}

MenuItemList MultiClipContextMenuModel::makeItems()
{
    MenuItemList cutAndItems {
        makeMenuItem(trackedit::TRACKEDIT_CUT_LEAVE_GAP_COMMAND,
                     muse::TranslatableString("multiclip", "Cut and leave gap")),
        makeMenuItem(trackedit::TRACKEDIT_CUT_PER_TRACK_RIPPLE_COMMAND,
                     muse::TranslatableString("multiclip", "Cut and close gap on this track")),
        makeMenuItem(trackedit::TRACKEDIT_CUT_ALL_TRACKS_RIPPLE_COMMAND,
                     muse::TranslatableString("multiclip", "Cut and close gap on all tracks")),
    };

    MenuItemList deleteAndItems {
        makeMenuItem(trackedit::TRACKEDIT_DELETE_LEAVE_GAP_COMMAND,
                     muse::TranslatableString("multiclip", "Delete and leave gap")),
        makeMenuItem(trackedit::TRACKEDIT_DELETE_PER_TRACK_RIPPLE_COMMAND,
                     muse::TranslatableString("multiclip", "Delete and close gap on this track")),
        makeMenuItem(trackedit::TRACKEDIT_DELETE_ALL_TRACKS_RIPPLE_COMMAND,
                     muse::TranslatableString("multiclip", "Delete and close gap on all tracks")),
    };

    MenuItemList items {
        makeMenuItem(trackedit::TRACKEDIT_CUT_COMMAND),
        makeMenuItem(trackedit::TRACKEDIT_COPY_COMMAND),
        makeMenuItem(trackedit::TRACKEDIT_DELETE_COMMAND),
        makeSeparator(),
        makeMenu(muse::TranslatableString("multiclip", "Cut and…"), cutAndItems, "menu-cut-and"),
        makeMenu(muse::TranslatableString("multiclip", "Delete and…"), deleteAndItems, "menu-delete-and"),
        makeSeparator(),
        makeMenuItem(trackedit::TRACKEDIT_JOIN_COMMAND,
                     //: Verb: context menu command that joins the selected clips
                     muse::TranslatableString("multiclip", "Join")),
        makeSeparator(),
        makeMenuItem(trackedit::TRACKEDIT_GROUP_ITEMS_COMMAND),
        makeMenuItem(trackedit::TRACKEDIT_UNGROUP_ITEMS_COMMAND),
    };

    return items;
}
