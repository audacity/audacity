#include "trackslistmodel.h"

#include "log.h"

using namespace au::projectscene;
using namespace au::project;
using namespace au::processing;

void TracksListModel::init()
{
    globalContext()->currentProcessingProjectChanged().onNotify(this, [this]() {
        reload();
    });
}

void TracksListModel::reload()
{
    //! NOTE Implimentation just for test

    beginResetModel();

    ProcessingProjectPtr prj = globalContext()->currentProcessingProject();
    if (!prj) {
        return;
    }

    const TrackList& tracks = prj->trackList();

    m_items.clear();

    for (const Track& t : tracks) {
        QVariantMap it;
        it["id"] = t.id.toQString();
        it["title"] = t.title.toQString();

        m_items << it;
    }

    endResetModel();
}

QHash<int, QByteArray> TracksListModel::roleNames() const
{
    static const QHash<int, QByteArray> roles = {
        { rItemData, "itemData" }
    };

    return roles;
}

QVariant TracksListModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid()) {
        return QVariant();
    }

    switch (role) {
    case rItemData: return m_items.at(index.row());
    }

    return QVariant();
}

int TracksListModel::rowCount(const QModelIndex& parent) const
{
    UNUSED(parent);
    return m_items.count();
}
