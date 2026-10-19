/**********************************************************************

  Audacity: A Digital Audio Editor

**********************************************************************/

#include "canvascontextmenumodel.h"

#include "trackedit/trackeditcommands.h"

using namespace au::projectscene;
using namespace muse;
using namespace muse::uicomponents;

void CanvasContextMenuModel::load()
{
    AbstractMenuModel::load();

    MenuItemList items = makeItems();
    setItems(items);
}

MenuItemList CanvasContextMenuModel::makeItems()
{
    MenuItemList pasteAndItems {
        makeMenuItem(trackedit::TRACKEDIT_PASTE_OVERLAP_COMMAND,
                     muse::TranslatableString("canvas", "Paste and overlap")),
        makeMenuItem(trackedit::TRACKEDIT_PASTE_INSERT_COMMAND,
                     muse::TranslatableString("canvas", "Paste and make room on this track")),
        makeMenuItem(trackedit::TRACKEDIT_PASTE_INSERT_ALL_TRACKS_RIPPLE_COMMAND,
                     muse::TranslatableString("canvas", "Paste and make room on all tracks")),
    };

    MenuItemList items {
        makeMenuItem(trackedit::TRACKEDIT_PASTE_DEFAULT_COMMAND),
        makeMenu(muse::TranslatableString("canvas", "Paste and…"), pasteAndItems, "menu-paste-and"),
        makeSeparator(),
        makeMenuItem(trackedit::TRACKEDIT_NEW_MONO_TRACK_COMMAND),
        makeMenuItem(trackedit::TRACKEDIT_NEW_STEREO_TRACK_COMMAND),
        makeMenuItem(trackedit::TRACKEDIT_NEW_LABEL_TRACK_COMMAND),
        makeSeparator(),
        makeMenuItem(trackedit::TRACKEDIT_SELECT_ALL_COMMAND),
        makeMenuItem(trackedit::TRACKEDIT_CLEAR_SELECTION_COMMAND)
    };

    return items;
}
