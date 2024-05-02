/*
* Audacity: A Digital Audio Editor
*/
#include "projecttoolbarmodel.h"

using namespace au::projectscene;
using namespace muse::uicomponents;

void ProjectToolBarModel::load()
{
    MenuItemList items = {
        makeItem("toggle-mixer"),
        makeItem("audio-setup"),
    };

    setItems(items);

    context()->currentProjectChanged().onNotify(this, [this]() {
        load();
    });

    AbstractMenuModel::load();
}

MenuItem* ProjectToolBarModel::makeItem(const muse::actions::ActionCode& actionCode)
{
    MenuItem* item = new MenuItem(actionsRegister()->action(actionCode), this);

    muse::ui::UiActionState state;
    state.enabled = context()->currentProject() != nullptr;
    item->setState(state);

    return item;
}
