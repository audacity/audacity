#include "tracksclipsmodel.h"

using namespace au::projectscene;

TracksClipsModel::TracksClipsModel(QObject* parent)
    : QAbstractListModel(parent)
{
}

void TracksClipsModel::load()
{
    au::processing::ProcessingProjectPtr prj = globalContext()->currentProcessingProject();
    if (!prj) {
        return;
    }

    beginResetModel();

    m_trackList = prj->trackList();

    //! TODO Subscribe

    endResetModel();
}

int TracksClipsModel::rowCount(const QModelIndex&) const
{
    return static_cast<int>(m_trackList.size());
}

QVariant TracksClipsModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid()) {
        return QVariant();
    }

    const au::processing::Track& track = m_trackList.at(index.row());
    switch (role) {
    case TrackIdRole:
        return QVariant::fromValue(track.id);
    default:
        break;
    }

    return QVariant();
}

QHash<int, QByteArray> TracksClipsModel::roleNames() const
{
    static QHash<int, QByteArray> roles
    {
        //{ TypeRole, "typeData" },
        { TrackIdRole, "trackIdData" }
    };
    return roles;
}
