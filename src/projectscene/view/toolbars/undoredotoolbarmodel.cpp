/*
* Audacity: A Digital Audio Editor
*/
#include "undoredotoolbarmodel.h"

#include "uicomponents/view/toolbaritem.h"

using namespace au::projectscene;
using namespace muse::uicomponents;
using namespace muse::actions;

void UndoRedoToolBarModel::load()
{
    AbstractToolBarModel::load();

    muse::actions::ActionCodeList itemsCodes = {
        "undo",
        "redo",
    };

    ToolBarItemList items;
    for (const ActionCode& code : itemsCodes) {
        ToolBarItem* item = makeItem(code);
        item->setIsTransparent(true);
        items << item;
    }

    setItems(items);

    context()->currentProjectChanged().onNotify(this, [this]() {
        load();
    });
}
