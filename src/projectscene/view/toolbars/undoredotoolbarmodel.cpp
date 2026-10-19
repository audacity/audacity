/*
* Audacity: A Digital Audio Editor
*/
#include "undoredotoolbarmodel.h"

#include "uicomponents/qml/Muse/UiComponents/toolbaritem.h"

#include "trackedit/trackeditcommands.h"

using namespace au::projectscene;
using namespace muse::uicomponents;
using namespace muse::rcommand;

void UndoRedoToolBarModel::load()
{
    if (m_loaded) {
        return;
    }

    ToolBarItemList items;
    for (const Command& command : { trackedit::TRACKEDIT_UNDO_COMMAND, trackedit::TRACKEDIT_REDO_COMMAND }) {
        ToolBarItem* item = makeItem(command);
        item->setIsTransparent(true);
        items << item;
    }

    setItems(items);

    AbstractToolBarModel::load();

    context()->currentProjectChanged().onNotify(this, [this]() {
        updateItems();

        subscribeOnHistoryChanges();
    });

    subscribeOnHistoryChanges();

    m_loaded = true;
}

void UndoRedoToolBarModel::onCommandStateChanged(const Command& command, const CommandState& state)
{
    updateTitle(command);

    AbstractToolBarModel::onCommandStateChanged(command, state);
}

void UndoRedoToolBarModel::updateTitle(const Command& command)
{
    auto history = projectHistory();

    if (command == trackedit::TRACKEDIT_UNDO_COMMAND) {
        ToolBarItem* undoItem = findItemPtr(trackedit::TRACKEDIT_UNDO_COMMAND);
        if (undoItem) {
            const muse::TranslatableString undoActionName = history ? history->topMostUndoActionName() : muse::TranslatableString();
            undoItem->setTitle(undoActionName.isEmpty()
                               ? muse::TranslatableString("action", "Undo")
                               : muse::TranslatableString("action", "Undo ‘%1’").arg(undoActionName));
        }
    } else if (command == trackedit::TRACKEDIT_REDO_COMMAND) {
        ToolBarItem* redoItem = findItemPtr(trackedit::TRACKEDIT_REDO_COMMAND);
        if (redoItem) {
            const muse::TranslatableString redoActionName = history ? history->topMostRedoActionName() : muse::TranslatableString();
            redoItem->setTitle(redoActionName.isEmpty()
                               ? muse::TranslatableString("action", "Redo")
                               : muse::TranslatableString("action", "Redo ‘%1’").arg(redoActionName));
        }
    }
}

void UndoRedoToolBarModel::updateItems()
{
    updateTitle(trackedit::TRACKEDIT_UNDO_COMMAND);
    updateTitle(trackedit::TRACKEDIT_REDO_COMMAND);
}

void UndoRedoToolBarModel::subscribeOnHistoryChanges()
{
    auto history = projectHistory();
    if (!history) {
        return;
    }

    history->historyChanged().onReceive(this, [this](const trackedit::HistoryEvent&) {
        updateItems();
    }, Asyncable::Mode::SetReplace);
}
