/*
* Audacity: A Digital Audio Editor
*/
#include "undoredotoolbarmodel.h"

#include "uicomponents/qml/Muse/UiComponents/toolbaritem.h"

using namespace au::projectscene;
using namespace muse::uicomponents;
using namespace muse::actions;

static const muse::actions::ActionCode UNDO_ACTION_CODE("action://undo");
static const muse::actions::ActionCode REDO_ACTION_CODE("action://redo");

void UndoRedoToolBarModel::load()
{
    if (m_loaded) {
        return;
    }

    muse::actions::ActionCodeList itemsCodes = {
        UNDO_ACTION_CODE,
        REDO_ACTION_CODE,
    };

    ToolBarItemList items;
    for (const ActionCode& code : itemsCodes) {
        ToolBarItem* item = makeItem(code);
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

void UndoRedoToolBarModel::onActionsStateChanges(const muse::actions::ActionCodeList& codes)
{
    auto history = projectHistory();

    for (const muse::actions::ActionCode& code : codes) {
        if (code == UNDO_ACTION_CODE) {
            ToolBarItem* undoItem = findItemPtr(UNDO_ACTION_CODE);
            if (undoItem) {
                const muse::TranslatableString undoActionName = history ? history->topMostUndoActionName() : muse::TranslatableString();
                undoItem->setTitle(undoActionName.isEmpty()
                                   ? muse::TranslatableString("action", "Undo")
                                   : muse::TranslatableString("action", "Undo ‘%1’").arg(undoActionName));
            }
        } else if (code == REDO_ACTION_CODE) {
            ToolBarItem* redoItem = findItemPtr(REDO_ACTION_CODE);
            if (redoItem) {
                const muse::TranslatableString redoActionName = history ? history->topMostRedoActionName() : muse::TranslatableString();
                redoItem->setTitle(redoActionName.isEmpty()
                                   ? muse::TranslatableString("action", "Redo")
                                   : muse::TranslatableString("action", "Redo ‘%1’").arg(redoActionName));
            }
        }
    }

    AbstractToolBarModel::onActionsStateChanges(codes);
}

void UndoRedoToolBarModel::updateItems()
{
    onActionsStateChanges({ UNDO_ACTION_CODE, REDO_ACTION_CODE });
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
