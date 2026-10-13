/*
* Audacity: A Digital Audio Editor
*/
#include "macromanagermodel.h"

using namespace au::macromanager;
using namespace muse;

static const UriQuery NEW_MACRO_URI("audacity://macromanager/newmacro");

MacroManagerModel::MacroManagerModel(QObject* parent)
    : QAbstractListModel(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

QVariant MacroManagerModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid() || index.row() >= rowCount()) {
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
    const RetVal<Val> result = interactive()->openSync(NEW_MACRO_URI);
    if (!result.ret) {
        return;
    }

    const QString title = result.val.toQString().trimmed();
    if (title.isEmpty()) {
        return;
    }

    const int row = m_items.size();

    beginInsertRows(QModelIndex(), row, row);
    m_items.append({ title });
    endInsertRows();

    emit isEmptyChanged();
}

void MacroManagerModel::run(int row)
{
    if (row < 0 || row >= m_items.size()) {
        return;
    }
}
