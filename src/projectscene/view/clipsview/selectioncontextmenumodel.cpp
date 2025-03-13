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
        makeMenuItem("cut"),
        makeMenuItem("cut-per-clip-ripple"),
        makeMenuItem("cut-per-track-ripple"),
        makeMenuItem("cut-all-tracks-ripple"),
        makeSeparator(),
        makeMenuItem("clip-copy-selected"),
        makeMenuItem("duplicate"),
        makeMenuItem("paste"),
        makeSeparator(),
        makeMenuItem("split")
    };

    return items;
}
