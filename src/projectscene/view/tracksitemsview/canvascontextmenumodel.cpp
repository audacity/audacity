/**********************************************************************

  Audacity: A Digital Audio Editor

**********************************************************************/

#include "canvascontextmenumodel.h"

using namespace au::projectscene;
using namespace muse;
using namespace muse::uicomponents;
using namespace muse::actions;

void CanvasContextMenuModel::load()
{
    AbstractMenuModel::load();

    MenuItemList items = makeItems();
    setItems(items);
}

MenuItemList CanvasContextMenuModel::makeItems()
{
    MenuItemList pasteAndItems {
        makeMenuItem("action://trackedit/paste-overlap",
                     muse::TranslatableString("canvas", "Paste and overlap")),
        makeMenuItem("action://trackedit/paste-insert",
                     muse::TranslatableString("canvas", "Paste and make room on this track")),
        makeMenuItem("action://trackedit/paste-insert-all-tracks-ripple",
                     muse::TranslatableString("canvas", "Paste and make room on all tracks")),
    };

    MenuItemList items {
        makeMenuItem("action://trackedit/paste-default"),
        makeMenu(muse::TranslatableString("canvas", "Paste and…"), pasteAndItems, "menu-paste-and"),
        makeSeparator(),
        makeMenuItem("new-mono-track"),
        makeMenuItem("new-stereo-track"),
        makeMenuItem("new-label-track"),
        makeSeparator(),
        makeMenuItem("select-all"),
        makeMenuItem("clear-selection")
    };

    return items;
}
