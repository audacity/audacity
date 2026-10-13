/*
* Audacity: A Digital Audio Editor
*/
#include "macromanagermodel.h"

#include "translation.h"

using namespace au::macromanager;
using namespace muse;

namespace {
const Uri MACRO_NAME_URI("audacity://macromanager/macroname");
const Uri DELETE_MACRO_URI("audacity://macromanager/deletemacro");
const Uri EDIT_MACRO_URI("audacity://macromanager/editmacro");
}

MacroManagerModel::MacroManagerModel(QObject* parent)
    : QAbstractListModel(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

QVariant MacroManagerModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid() || !isRowValid(index.row())) {
        return QVariant();
    }

    const Item& item = m_items.at(index.row());

    switch (role) {
    case TitleRole: return item.title;
    default: return QVariant();
    }
}

int MacroManagerModel::rowCount(const QModelIndex&) const
{
    return m_items.size();
}

QHash<int, QByteArray> MacroManagerModel::roleNames() const
{
    static const QHash<int, QByteArray> roles {
        { TitleRole, "title" }
    };

    return roles;
}

bool MacroManagerModel::isEmpty() const
{
    return m_items.isEmpty();
}

void MacroManagerModel::init()
{
    beginResetModel();
    m_items.clear();
    endResetModel();

    emit isEmptyChanged();
}

void MacroManagerModel::newMacro()
{
    const std::optional<QString> title = requestMacroName(UriQuery(MACRO_NAME_URI));
    if (!title) {
        return;
    }

    const int row = m_items.size();

    beginInsertRows(QModelIndex(), row, row);
    m_items.append({ *title });
    endInsertRows();

    emit isEmptyChanged();

    edit(row);
}

void MacroManagerModel::edit(int row)
{
    if (!isRowValid(row)) {
        return;
    }

    UriQuery uri(EDIT_MACRO_URI);
    uri.addParam("macroName", Val(m_items.at(row).title));

    interactive()->open(uri);
}

void MacroManagerModel::rename(int row)
{
    if (!isRowValid(row)) {
        return;
    }

    UriQuery uri(MACRO_NAME_URI);
    uri.addParam("title", Val(qtrc("macromanager", "Rename macro")));
    uri.addParam("macroName", Val(m_items.at(row).title));

    const std::optional<QString> title = requestMacroName(uri);
    if (!title) {
        return;
    }

    m_items[row].title = *title;

    const QModelIndex idx = index(row);
    emit dataChanged(idx, idx, { TitleRole });
}

void MacroManagerModel::remove(int row)
{
    if (!isRowValid(row)) {
        return;
    }

    UriQuery uri(DELETE_MACRO_URI);
    uri.addParam("macroName", Val(m_items.at(row).title));

    const RetVal<Val> result = interactive()->openSync(uri);
    if (!result.ret) {
        return;
    }

    beginRemoveRows(QModelIndex(), row, row);
    m_items.removeAt(row);
    endRemoveRows();

    emit isEmptyChanged();
}

void MacroManagerModel::run(int row)
{
    if (!isRowValid(row)) {
        return;
    }
}

bool MacroManagerModel::isRowValid(int row) const
{
    return row >= 0 && row < m_items.size();
}

std::optional<QString> MacroManagerModel::requestMacroName(const UriQuery& uri) const
{
    const RetVal<Val> result = interactive()->openSync(uri);
    if (!result.ret) {
        return std::nullopt;
    }

    const QString title = result.val.toQString().trimmed();
    if (title.isEmpty()) {
        return std::nullopt;
    }

    return title;
}
