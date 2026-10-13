/*
* Audacity: A Digital Audio Editor
*/
#include "macrocommandsmodel.h"

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
    const int row = m_items.size();

    beginInsertRows(QModelIndex(), row, row);
    m_items.append({ code, title });
    endInsertRows();
}

void MacroCommandsModel::remove(int row)
{
    if (!isRowValid(row)) {
        return;
    }

    beginRemoveRows(QModelIndex(), row, row);
    m_items.removeAt(row);
    endRemoveRows();

    if (row < m_items.size()) {
        emit dataChanged(index(row), index(m_items.size() - 1), { StepRole });
    }
}

bool MacroCommandsModel::isRowValid(int row) const
{
    return row >= 0 && row < m_items.size();
}
