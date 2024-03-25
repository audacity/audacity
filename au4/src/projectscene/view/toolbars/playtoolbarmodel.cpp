#include "playtoolbarmodel.h"

using namespace mu::uicomponents;
using namespace au::projectscene;

void PlayToolBarModel::load()
{
    MenuItemList items = {
        makeItem("play"),
        makeItem("stop"),
        makeItem("rewind"),
    };

    setItems(items);

    AbstractMenuModel::load();
}

MenuItem* PlayToolBarModel::makeItem(const mu::actions::ActionCode& actionCode)
{
    MenuItem* item = new MenuItem(actionsRegister()->action(actionCode), this);

    item->setState(mu::ui::UiActionState::make_enabled());

    return item;
}
