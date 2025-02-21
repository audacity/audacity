/*
* Audacity: A Digital Audio Editor
*/

#include "historypanelmodel.h"

using namespace au::projectscene;
using namespace muse;

HistoryPanelModel::HistoryPanelModel(QObject* parent)
    : QAbstractListModel(parent), Injectable(iocCtxForQmlObject(this))
{
}

void HistoryPanelModel::classBegin()
{
    onCurrentProjectChanged();

    context()->currentProjectChanged().onNotify(this, [this] {
        onCurrentProjectChanged();
    });

    projectHistory()->historyChanged().onNotify(this, [this] {
        onUndoRedo();
    });
}

bool HistoryPanelModel::isProjectLoaded() const
{
    return context()->currentProject() != nullptr;
}

void HistoryPanelModel::onCurrentProjectChanged()
{
    beginResetModel();
    m_rowCount = isProjectLoaded() ? std::max(projectHistory()->undoRedoActionCount(), static_cast<size_t>(1)) : 0;
    endResetModel();

    emit currentIndexChanged();
}

void HistoryPanelModel::onUndoRedo()
{
    int newRowCount = projectHistory()->undoRedoActionCount();

    if (m_rowCount < newRowCount) {
        beginInsertRows(QModelIndex(), m_rowCount, newRowCount - 1);
        m_rowCount = newRowCount;
        endInsertRows();
    } else if (m_rowCount > newRowCount) {
        beginRemoveRows(QModelIndex(), newRowCount, m_rowCount - 1);
        m_rowCount = newRowCount;
        endRemoveRows();
    }

    // When performing a new action after undoing one or more actions, the
    // redo stack is cleared, and the new action is pushed onto the stack;
    // that means that the item at the current index now represents the new
    // action, rather than the action on the redo stack.
    int newCurrentIndex = projectHistory()->currentStateIndex();
    emit dataChanged(index(newCurrentIndex), index(newCurrentIndex));

    emit currentIndexChanged();
}

QVariant HistoryPanelModel::data(const QModelIndex& index, int role) const
{
    int row = index.row();
    if (row < 0 || row >= int(projectHistory()->undoRedoActionCount()) + 1) {
        return {};
    }

    switch (role) {
    case Qt::DisplayRole:
        if (row == 0) {
            return qtrc("project/history", "File opened");
        }
        return projectHistory()->lastActionNameAtIdx(static_cast<size_t>(row)).qTranslated();
    default:
        return {};
    }
}

int HistoryPanelModel::rowCount(const QModelIndex&) const
{
    return m_rowCount;
}

QHash<int, QByteArray> HistoryPanelModel::roleNames() const
{
    return { { Qt::DisplayRole, "text" } };
}

int HistoryPanelModel::currentIndex() const
{
    return isProjectLoaded() ? projectHistory()->currentStateIndex() : 0;
}

void HistoryPanelModel::undoRedoToIndex(int index)
{
    trackeditInteraction()->undoRedoToIndex(static_cast<size_t>(index));
}
