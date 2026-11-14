/**********************************************************************

  Audacity: A Digital Audio Editor

**********************************************************************/

#include "selectioncontextmenumodel.h"

#include "trackedit/trackeditcommands.h"

using namespace au::projectscene;
using namespace muse;
using namespace muse::uicomponents;

void SelectionContextMenuModel::load()
{
    AbstractMenuModel::load();

    MenuItemList items = makeItems();
    setItems(items);
}

MenuItemList SelectionContextMenuModel::makeItems()
{
    MenuItemList cutAndItems {
        makeMenuItem(trackedit::TRACKEDIT_CUT_LEAVE_GAP_COMMAND,
                     muse::TranslatableString("selection", "Cut and leave gap")),
        makeMenuItem(trackedit::TRACKEDIT_CUT_PER_CLIP_RIPPLE_COMMAND,
                     muse::TranslatableString("selection", "Cut and close gap")),
        makeMenuItem(trackedit::TRACKEDIT_CUT_PER_TRACK_RIPPLE_COMMAND,
                     muse::TranslatableString("selection", "Cut and close gap on this track")),
        makeMenuItem(trackedit::TRACKEDIT_CUT_ALL_TRACKS_RIPPLE_COMMAND,
                     muse::TranslatableString("selection", "Cut and close gap on all tracks")),
    };

    MenuItemList pasteAndItems {
        makeMenuItem(trackedit::TRACKEDIT_PASTE_OVERLAP_COMMAND,
                     muse::TranslatableString("selection", "Paste and overlap")),
        makeMenuItem(trackedit::TRACKEDIT_PASTE_INSERT_COMMAND,
                     muse::TranslatableString("selection", "Paste and make room on this track")),
        makeMenuItem(trackedit::TRACKEDIT_PASTE_INSERT_ALL_TRACKS_RIPPLE_COMMAND,
                     muse::TranslatableString("selection", "Paste and make room on all tracks")),
    };

    MenuItemList deleteAndItems {
        makeMenuItem(trackedit::TRACKEDIT_DELETE_LEAVE_GAP_COMMAND,
                     muse::TranslatableString("selection", "Delete and leave gap")),
        makeMenuItem(trackedit::TRACKEDIT_DELETE_PER_CLIP_RIPPLE_COMMAND,
                     muse::TranslatableString("selection", "Delete and close gap")),
        makeMenuItem(trackedit::TRACKEDIT_DELETE_PER_TRACK_RIPPLE_COMMAND,
                     muse::TranslatableString("selection", "Delete and close gap on this track")),
        makeMenuItem(trackedit::TRACKEDIT_DELETE_ALL_TRACKS_RIPPLE_COMMAND,
                     muse::TranslatableString("selection", "Delete and close gap on all tracks")),
    };

    MenuItemList items {
        makeMenuItem(trackedit::TRACKEDIT_CUT_COMMAND),
        makeMenuItem(trackedit::TRACKEDIT_COPY_COMMAND),
        makeMenuItem(trackedit::TRACKEDIT_PASTE_DEFAULT_COMMAND),
        makeMenuItem(trackedit::TRACKEDIT_DUPLICATE_COMMAND),
        makeMenuItem(trackedit::TRACKEDIT_DELETE_COMMAND),
        makeSeparator(),
        makeMenu(muse::TranslatableString("selection", "Cut and…"), cutAndItems, "menu-cut-and"),
        makeMenu(muse::TranslatableString("selection", "Paste and…"), pasteAndItems, "menu-paste-and"),
        makeMenu(muse::TranslatableString("selection", "Delete and…"), deleteAndItems, "menu-delete-and"),
        makeSeparator(),
        makeMenuItem(trackedit::TRACKEDIT_SPLIT_COMMAND),
        makeMenuItem(trackedit::TRACKEDIT_DISJOIN_COMMAND,
                     muse::TranslatableString("selection", "Split at silences")),
        makeMenuItem(trackedit::TRACKEDIT_JOIN_COMMAND,
                     //: Verb: context menu command that joins the selected audio
                     muse::TranslatableString("selection", "Join"))
    };

    return items;
}
