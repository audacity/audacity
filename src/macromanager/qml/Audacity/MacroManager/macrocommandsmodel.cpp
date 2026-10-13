/*
* Audacity: A Digital Audio Editor
*/
#include "macrocommandsmodel.h"

#include <algorithm>

using namespace au::macromanager;

MacroCommandsModel::MacroCommandsModel(QObject* parent)
    : QAbstractListModel(parent)
{
}

QVariant MacroCommandsModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid() || !isRowValid(index.row())) {
        return QVariant();
    }

    const Item& item = m_items.at(index.row());

    switch (role) {
    case StepRole: return index.row() + 1;
    case CodeRole: return item.code;
    case TitleRole: return item.title;
    default: return QVariant();
    }
}

int MacroCommandsModel::rowCount(const QModelIndex&) const
{
    return m_items.size();
}

QHash<int, QByteArray> MacroCommandsModel::roleNames() const
{
    static const QHash<int, QByteArray> roles {
        { StepRole, "step" },
        { CodeRole, "code" },
        { TitleRole, "title" }
    };

    return roles;
}

void MacroCommandsModel::add(const QString& code, const QString& title)
{
    insert(m_items.size(), code, title);
}

void MacroCommandsModel::insert(int row, const QString& code, const QString& title)
{
    row = std::clamp<int>(row, 0, m_items.size());

    beginInsertRows(QModelIndex(), row, row);
    m_items.insert(row, { code, title });
    endInsertRows();

    notifyStepsChanged(row + 1, m_items.size() - 1);
}

void MacroCommandsModel::move(int from, int to)
{
    to = std::clamp<int>(to, 0, m_items.size() - 1);

    if (!isRowValid(from) || from == to) {
        return;
    }

    beginMoveRows(QModelIndex(), from, from, QModelIndex(), to > from ? to + 1 : to);
    m_items.move(from, to);
    endMoveRows();

    notifyStepsChanged(std::min(from, to), std::max(from, to));
}

void MacroCommandsModel::remove(int row)
{
    if (!isRowValid(row)) {
        return;
    }

    beginRemoveRows(QModelIndex(), row, row);
    m_items.removeAt(row);
    endRemoveRows();

    notifyStepsChanged(row, m_items.size() - 1);
}

bool MacroCommandsModel::isRowValid(int row) const
{
    return row >= 0 && row < m_items.size();
}

void MacroCommandsModel::notifyStepsChanged(int fromRow, int toRow)
{
    if (fromRow > toRow || !isRowValid(fromRow) || !isRowValid(toRow)) {
        return;
    }

    emit dataChanged(index(fromRow), index(toRow), { StepRole });
}
