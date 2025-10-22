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
    MenuItemList items {
        makeMenuItem("action://trackedit/cut"),
        makeMenuItem("action://trackedit/copy"),
        makeMenuItem("action://trackedit/delete"),
        makeSeparator(),
        makeMenuItem("group-clips"),
        makeMenuItem("ungroup-clips")
    };

    return items;
}
