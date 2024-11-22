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
    MenuItemList items {
        makeMenuItem("new-mono-track"),
        makeMenuItem("new-stereo-track"),
        makeMenuItem("new-label-track"),
        makeSeparator(),
        makeMenuItem("show-master-track"),
        makeSeparator(),
        makeMenuItem("select-all"),
        makeMenuItem("clear-selection")
    };

    return items;
}
