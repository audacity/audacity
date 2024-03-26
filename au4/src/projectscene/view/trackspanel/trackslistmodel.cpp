#include "trackslistmodel.h"

#include "log.h"

using namespace au::projectscene;
using namespace mu::project;
using namespace au::processing;

void TracksListModel::load()
{
    ProcessingProjectPtr prj = globalContext()->currentProcessingProject();
    if (!prj) {
        return;
    }

    const TrackList& tracks = prj->trackList();
    UNUSED(tracks);

    //! TODO Make view items
    //! TODO subscribe on changes
}

QHash<int, QByteArray> TracksListModel::roleNames() const
{
    static const QHash<int, QByteArray> roles = {
        { TitleRole, "titleRole" }
    };
    return roles;
}

QVariant TracksListModel::data(const QModelIndex& index, int role) const
{
    UNUSED(index);
    UNUSED(role);
    return QVariant();
}

int TracksListModel::rowCount(const QModelIndex& parent) const
{
    UNUSED(parent);
    return 0;
}
