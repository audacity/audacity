/*
* Audacity: A Digital Audio Editor
*/
#include "trackslistclipsmodel.h"

using namespace au::projectscene;

TracksListClipsModel::TracksListClipsModel(QObject* parent)
    : QAbstractListModel(parent)
{
    configuration()->isVerticalRulersVisibleChanged().onReceive(this, [this](bool isVerticalRulersVisible){
        setIsVerticalRulersVisible(isVerticalRulersVisible);
    });
}

void TracksListClipsModel::load()
{
    au::processing::ProcessingProjectPtr prj = globalContext()->currentProcessingProject();
    if (!prj) {
        return;
    }

    setIsVerticalRulersVisible(configuration()->isVerticalRulersVisible());

    beginResetModel();

    m_trackList = prj->trackList();

    //! TODO Subscribe on tracks changed

    muse::ValCh<std::vector<processing::TrackId> > dataSelectedTracks = processingSelectionController()->dataSelectedOnTracks();
    m_dataSelectedTracks = { dataSelectedTracks.val.cbegin(), dataSelectedTracks.val.cend() };
    dataSelectedTracks.ch.onReceive(this, [this](const std::vector<processing::TrackId>& tracks) {
        setDataSelectedTracks(tracks);
    });

    endResetModel();
}

int TracksListClipsModel::rowCount(const QModelIndex&) const
{
    return static_cast<int>(m_trackList.size());
}

QVariant TracksListClipsModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid()) {
        return QVariant();
    }

    const au::processing::Track& track = m_trackList.at(index.row());
    switch (role) {
    case TrackIdRole:
        return QVariant::fromValue(track.id);
    case IsDataSelectedRole: {
        return m_dataSelectedTracks.contains(track.id);
    }
    default:
        break;
    }

    return QVariant();
}

QHash<int, QByteArray> TracksListClipsModel::roleNames() const
{
    static QHash<int, QByteArray> roles
    {
        //{ TypeRole, "trackType" },
        { TrackIdRole, "trackId" },
        { IsDataSelectedRole, "isDataSelected" }
    };
    return roles;
}

bool TracksListClipsModel::isVerticalRulersVisible() const
{
    return m_isVerticalRulersVisible;
}

void TracksListClipsModel::setIsVerticalRulersVisible(bool isVerticalRulersVisible)
{
    if (m_isVerticalRulersVisible == isVerticalRulersVisible) {
        return;
    }

    m_isVerticalRulersVisible = isVerticalRulersVisible;
    emit isVerticalRulersVisibleChanged(m_isVerticalRulersVisible);
}

QList<int> TracksListClipsModel::dataSelectedTracks() const
{
    return m_dataSelectedTracks;
}

void TracksListClipsModel::setDataSelectedTracks(const std::vector<processing::TrackId>& tracks)
{
    QList<int> l = { tracks.cbegin(), tracks.cend() };
    setDataSelectedTracks(l);
}

void TracksListClipsModel::setDataSelectedTracks(const QList<int>& newDataSelectedTracks)
{
    if (m_dataSelectedTracks == newDataSelectedTracks) {
        return;
    }
    m_dataSelectedTracks = newDataSelectedTracks;
    emit dataSelectedTracksChanged();
    emit dataChanged(index(0), index(m_trackList.size() - 1), { IsDataSelectedRole });
}
