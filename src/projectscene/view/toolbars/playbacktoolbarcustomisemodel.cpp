/*
* Audacity: A Digital Audio Editor
*/
#include "playbacktoolbarcustomisemodel.h"

#include <QList>

#include "internal/projectsceneuiactions.h"

#include "playbacktoolbarcustomiseitem.h"

#include "translation.h"
#include "log.h"

using namespace au::projectscene;
using namespace muse;
using namespace muse::ui;
using namespace muse::uicomponents;
using namespace muse::actions;

static const QString TOOLBAR_NAME("playbackToolBar");

static const ActionQuery PLAYBACK_PLAY_QUERY("action://playback/play");
static const ActionQuery RECORD_START_QUERY("action://record/start");

PlaybackToolBarCustomiseModel::PlaybackToolBarCustomiseModel(QObject* parent)
    : SelectableItemListModel(parent)
{
}

void PlaybackToolBarCustomiseModel::load()
{
    TRACEFUNC;

    QList<Item*> items;

    ToolConfig toolConfig = uiConfiguration()->toolConfig(TOOLBAR_NAME,
                                                          projectscene::ProjectSceneUiActions::defaultPlaybackToolBarConfig());

    auto isSeparator = [](const UiAction& a) { return a.code.empty(); };

    bool lastWasSeparator = false;
    for (const auto& configItem : toolConfig.items) {
        UiAction action = actionsRegister()->action(configItem.action);

        if (isSeparator(action) && lastWasSeparator) {
            continue;
        }
        lastWasSeparator = isSeparator(action);

        items << makeItem(action, configItem.show);
    }

    setItems(items);
}

QVariant PlaybackToolBarCustomiseModel::data(const QModelIndex& index, int role) const
{
    PlaybackToolBarCustomiseItem* item = modelIndexToItem(index);
    if (!item) {
        return QVariant();
    }

    switch (role) {
    case ItemRole: return QVariant::fromValue(item);
    default: break;
    }

    return SelectableItemListModel::data(index, role);
}

QHash<int, QByteArray> PlaybackToolBarCustomiseModel::roleNames() const
{
    QHash<int, QByteArray> roles = SelectableItemListModel::roleNames();
    roles[ItemRole] = "item";

    return roles;
}

void PlaybackToolBarCustomiseModel::addSeparatorLine()
{
    if (!hasSelection()) {
        return;
    }

    QModelIndex selectedItemIndex = selection()->selectedIndexes().first();
    if (!selectedItemIndex.isValid()) {
        return;
    }

    QModelIndex prevItemIndex = index(selectedItemIndex.row() - 1);
    if (!prevItemIndex.isValid()) {
        return;
    }

    insertItem(prevItemIndex.row() + 1, makeSeparatorItem());

    onUpdateOperationsAvailability();
    saveActions();
}

QItemSelectionModel* PlaybackToolBarCustomiseModel::selectionModel() const
{
    return selection();
}

bool PlaybackToolBarCustomiseModel::isAddSeparatorAvailable() const
{
    return m_isAddSeparatorAvailable;
}

void PlaybackToolBarCustomiseModel::setIsAddSeparatorAvailable(bool isAddSeparatorAvailable)
{
    if (m_isAddSeparatorAvailable == isAddSeparatorAvailable) {
        return;
    }

    m_isAddSeparatorAvailable = isAddSeparatorAvailable;
    emit isAddSeparatorAvailableChanged(m_isAddSeparatorAvailable);
}

PlaybackToolBarCustomiseItem* PlaybackToolBarCustomiseModel::modelIndexToItem(const QModelIndex& index) const
{
    return dynamic_cast<PlaybackToolBarCustomiseItem*>(item(index));
}

void PlaybackToolBarCustomiseModel::onUpdateOperationsAvailability()
{
    TRACEFUNC;

    SelectableItemListModel::onUpdateOperationsAvailability();
    updateRemovingAvailability();
    updateAddSeparatorAvailability();
}

void PlaybackToolBarCustomiseModel::onRowsMoved()
{
    saveActions();
}

void PlaybackToolBarCustomiseModel::onRowsRemoved()
{
    saveActions();
}

void PlaybackToolBarCustomiseModel::updateRemovingAvailability()
{
    TRACEFUNC;

    auto hasActionInSelection = [this](const QModelIndexList& selectedIndexes) {
        for (const QModelIndex& index : selectedIndexes) {
            const PlaybackToolBarCustomiseItem* item = modelIndexToItem(index);

            if (item && item->type() == PlaybackToolBarCustomiseItem::ACTION) {
                return true;
            }
        }

        return false;
    };

    QModelIndexList selectedIndexes = selection()->selectedIndexes();
    bool removingAvailable = !selectedIndexes.empty();

    if (removingAvailable) {
        removingAvailable = !hasActionInSelection(selectedIndexes);
    }

    setIsRemovingAvailable(removingAvailable);
}

void PlaybackToolBarCustomiseModel::updateAddSeparatorAvailability()
{
    TRACEFUNC;

    bool addingAvailable = !selection()->selectedIndexes().empty();
    if (!addingAvailable) {
        setIsAddSeparatorAvailable(addingAvailable);
        return;
    }

    addingAvailable = selection()->selectedIndexes().count() == 1;
    if (!addingAvailable) {
        setIsAddSeparatorAvailable(addingAvailable);
        return;
    }

    QModelIndex selectedItemIndex = selection()->selectedIndexes().first();

    const PlaybackToolBarCustomiseItem* selectedItem = modelIndexToItem(selectedItemIndex);
    addingAvailable = selectedItem && selectedItem->type() == PlaybackToolBarCustomiseItem::ACTION;

    if (!addingAvailable) {
        setIsAddSeparatorAvailable(addingAvailable);
        return;
    }

    QModelIndex prevItemIndex = index(selectedItemIndex.row() - 1);
    addingAvailable = prevItemIndex.isValid();
    if (addingAvailable) {
        PlaybackToolBarCustomiseItem* prevItem = modelIndexToItem(prevItemIndex);
        addingAvailable = prevItem && prevItem->type() == PlaybackToolBarCustomiseItem::ACTION;
    }

    setIsAddSeparatorAvailable(addingAvailable);
}

PlaybackToolBarCustomiseItem* PlaybackToolBarCustomiseModel::makeItem(const UiAction& action, bool checked)
{
    if (action.code.empty()) {
        return makeSeparatorItem();
    }

    PlaybackToolBarCustomiseItem* item = new PlaybackToolBarCustomiseItem(PlaybackToolBarCustomiseItem::ItemType::ACTION, this);
    item->setId(QString::fromStdString(action.code));
    item->setTitle(action.title.qTranslatedWithoutMnemonic());
    item->setIcon(action.iconCode);
    item->setIconColor(iconColor(action));
    item->setChecked(checked);

    connect(item, &PlaybackToolBarCustomiseItem::checkedChanged, this, [this](bool) {
        saveActions();
    });

    return item;
}

PlaybackToolBarCustomiseItem* PlaybackToolBarCustomiseModel::makeSeparatorItem()
{
    PlaybackToolBarCustomiseItem* item = new PlaybackToolBarCustomiseItem(PlaybackToolBarCustomiseItem::ItemType::SEPARATOR, this);
    item->setTitle(QString("-------  %1  -------").arg(muse::qtrc("projectscene", "Separator line")));
    item->setChecked(true); //! NOTE Can't be unchecked
    return item;
}

QColor PlaybackToolBarCustomiseModel::iconColor(const muse::ui::UiAction& action) const
{
    QColor color = QColor(uiConfiguration()->currentTheme().values.value(muse::ui::FONT_PRIMARY_COLOR).toString());
    if (action.code == PLAYBACK_PLAY_QUERY.toString()) {
        color = uiConfiguration()->currentTheme().values.value(muse::ui::PLAY_COLOR).value<QColor>();
    } else if (action.code == RECORD_START_QUERY.toString()) {
        color = uiConfiguration()->currentTheme().values.value(muse::ui::RECORD_COLOR).value<QColor>();
    }

    return color;
}

void PlaybackToolBarCustomiseModel::saveActions()
{
    TRACEFUNC;

    ToolConfig config;
    for (const Item* item : items()) {
        auto customiseItem = dynamic_cast<const PlaybackToolBarCustomiseItem*>(item);
        if (!customiseItem) {
            continue;
        }

        ToolConfig::Item citem;
        citem.action = customiseItem->id().toStdString();
        citem.show = customiseItem->checked();
        config.items.append(citem);
    }

    uiConfiguration()->setToolConfig(TOOLBAR_NAME, config);
}
