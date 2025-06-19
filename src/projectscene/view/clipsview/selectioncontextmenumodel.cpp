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
        makeMenuItem("copy"),
        makeMenuItem("duplicate"),
        makeMenuItem("paste"),
        makeSeparator(),
        makeMenuItem("silence-audio-selection"),
        makeMenuItem("delete-per-track-ripple"),
        makeSeparator(),
        makeMenuItem("split"),
        makeMenuItem("split-into-new-track"),
        makeMenuItem("disjoin"),
        makeMenuItem("join"),
    };

    return items;
}
