/*
* Audacity: A Digital Audio Editor
*/
#include "playbacktoolbarmodel.h"

#include "view/projectsceneuiactions.h"

#include "log.h"

using namespace muse::uicomponents;
using namespace au::projectscene;

static const QString TOOLBAR_NAME("playbackToolBar");

PlaybackToolBarModel::PlaybackToolBarModel(QObject* parent)
    : AbstractMenuModel(parent)
{
    uiConfiguration()->toolConfigChanged(TOOLBAR_NAME).onNotify(this, [this]() {
        load();
    });

    context()->currentProjectChanged().onNotify(this, [this]() {
        onProjectChanged();
    });
}

void PlaybackToolBarModel::load()
{
    MenuItemList items;

    muse::ui::ToolConfig noteInputConfig
        = uiConfiguration()->toolConfig(TOOLBAR_NAME, ProjectSceneUiActions::defaultPlaybackToolBarConfig());

    int section = 0;
    for (const muse::ui::ToolConfig::Item& citem : noteInputConfig.items) {
        if (!citem.show) {
            continue;
        }

        if (citem.action.empty()) {
            section++;
            continue;
        }

        MenuItem* item = makeActionItem(uiActionsRegister()->action(citem.action), QString::number(section));

        items << item;
    }

    setItems(items);

    onProjectChanged();

    AbstractMenuModel::load();
}

QVariant PlaybackToolBarModel::data(const QModelIndex& index, int role) const
{
    TRACEFUNC;

    int row = index.row();

    if (!isIndexValid(row)) {
        return QVariant();
    }

    const MenuItem* item = items()[row];
    switch (role) {
    case IsMenuSecondaryRole: return isMenuSecondary(item->action().code);
    case OrderRole: return row;
    case SectionRole: return item->section();
    default: return AbstractMenuModel::data(index, role);
    }
}

QHash<int, QByteArray> PlaybackToolBarModel::roleNames() const
{
    QHash<int, QByteArray> roles = AbstractMenuModel::roleNames();
    roles[IsMenuSecondaryRole] = "isMenuSecondary";
    roles[OrderRole] = "order";
    roles[SectionRole] = "section";

    return roles;
}

void PlaybackToolBarModel::onProjectChanged()
{
    updateState();
}

void PlaybackToolBarModel::updateState()
{
    for (int i = 0; i < rowCount(); ++i) {
        MenuItem& item = this->item(i);
        muse::ui::UiActionState state = item.state();
        state.checked = false;
        item.setState(state);
    }
}

bool PlaybackToolBarModel::isMenuSecondary(const muse::actions::ActionCode& actionCode) const
{
    UNUSED(actionCode);

    return false;
}

MenuItem* PlaybackToolBarModel::makeActionItem(const muse::ui::UiAction& action, const QString& section,
                                               const muse::uicomponents::MenuItemList& subitems)
{
    MenuItem* item = new MenuItem(action, this);
    item->setSection(section);
    item->setSubitems(subitems);
    return item;
}
