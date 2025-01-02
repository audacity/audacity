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
        makeMenuItem("multi-clip-cut"),
        makeMenuItem("multi-clip-copy"),
        makeMenuItem("multi-clip-delete"),
        makeSeparator(),
        makeMenuItem("group-clips"),
        makeMenuItem("ungroup-clips")
    };

    return items;
}
