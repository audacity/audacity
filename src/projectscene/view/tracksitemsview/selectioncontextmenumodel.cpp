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
        makeMenuItem("action://trackedit/cut"),
        makeMenuItem("action://trackedit/copy"),
        makeMenuItem("action://trackedit/paste-default"),
        makeSeparator(),
        makeMenuItem("duplicate"),
        makeMenuItem("split")
    };

    return items;
}
