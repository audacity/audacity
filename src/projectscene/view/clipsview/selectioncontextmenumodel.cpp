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
    MenuItemList items {
        makeMenuItem("clip-cut-selected"),
        makeMenuItem("clip-copy-selected"),
        makeMenuItem("paste"),
        makeMenuItem("clip-delete-selected"),
        makeSeparator(),
        makeMenuItem("split-cut"),
        makeMenuItem("duplicate"),
        makeMenuItem("split-delete"),
        makeSeparator(),
        makeMenuItem("split")
    };

    return items;
}
