/*
* Audacity: A Digital Audio Editor
*/
#include "projecttoolbarmodel.h"

#include "uicomponents/view/toolbaritem.h"

using namespace au::projectscene;
using namespace muse::uicomponents;
using namespace muse::actions;

void ProjectToolBarModel::load()
{
    AbstractToolBarModel::load();

    muse::actions::ActionCodeList itemsCodes = {
        "toggle-mixer",
        "audio-setup",
    };

    ToolBarItemList items;
    for (const ActionCode& code : itemsCodes) {
        ToolBarItem* item = makeItem(code);
        item->setShowTitle(true);

        items << item;
    }

    setItems(items);

    context()->currentProjectChanged().onNotify(this, [this]() {
        load();
    });
}
