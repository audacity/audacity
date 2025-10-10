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
        // TODO: here the trackedit/ actions won't have shortcut keys associated with and wont show in the popup,
        // we could default to use the main action like action://cut but that doesn't make sense in terms of design,
        // the framework should evolve to support contextual shortcut actions
        makeMenuItem("action://trackedit/cut"),
        makeMenuItem("action://trackedit/copy"),
        makeMenuItem("action://trackedit/paste-default"),
        makeSeparator(),
        makeMenuItem("duplicate"),
        makeMenuItem("split")
    };

    return items;
}
