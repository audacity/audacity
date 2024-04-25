/*
* Audacity: A Digital Audio Editor
*/
#include "playbacktoolbarmodel.h"

#include "internal/playbackuiactions.h"

#include "log.h"

using namespace muse::uicomponents;
using namespace muse::ui;
using namespace muse::actions;
using namespace au::playback;

static const QString TOOLBAR_NAME("playbackToolBar");

static const ActionCode PLAY_ACTION_CODE("play");

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
    AbstractMenuModel::load();
    updateActions();
    setupConnections();
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

void PlaybackToolBarModel::onActionsStateChanges(const muse::actions::ActionCodeList& codes)
{
    AbstractMenuModel::onActionsStateChanges(codes);

    if (containsAction(codes, PLAY_ACTION_CODE)) {
        MenuItem& item = findItem(PLAY_ACTION_CODE);
        item.setAction(playAction());
    }
}

void PlaybackToolBarModel::setupConnections()
{
    controller()->isPlayingChanged().onNotify(this, [this]() {
        onActionsStateChanges({ PLAY_ACTION_CODE });
    });
}

void PlaybackToolBarModel::onProjectChanged()
{
    updateState();
}

void PlaybackToolBarModel::updateActions()
{
    MenuItemList items;

    muse::ui::ToolConfig playbackConfig
        = uiConfiguration()->toolConfig(TOOLBAR_NAME, PlaybackUiActions::defaultPlaybackToolConfig());

    int section = 0;
    for (const muse::ui::ToolConfig::Item& citem : playbackConfig.items) {
        if (!citem.show) {
            continue;
        }

        if (citem.action.empty()) {
            section++;
            continue;
        }

        MenuItem* item = makeActionItem(uiActionsRegister()->action(citem.action), QString::number(section));

        if (citem.action == PLAY_ACTION_CODE) {
            item->setAction(playAction());
        }

        items << item;
    }

    setItems(items);
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

UiAction PlaybackToolBarModel::playAction() const
{
    UiAction action = uiActionsRegister()->action(PLAY_ACTION_CODE);

    bool isPlaying = controller()->isPlaying();
    action.iconCode =  isPlaying ? IconCode::Code::PAUSE : IconCode::Code::PLAY;

    return action;
}
