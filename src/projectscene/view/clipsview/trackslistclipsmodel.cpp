/*
* Audacity: A Digital Audio Editor
*/
#include "trackslistclipsmodel.h"

#include "global/async/async.h"

#include "global/containers.h"

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
    au::trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return;
    }

    setIsVerticalRulersVisible(configuration()->isVerticalRulersVisible());

    beginResetModel();

    m_trackList = prj->trackList();

    m_dataSelectedTracks = selectionController()->dataSelectedOnTracks();
    m_selectedTrack = selectionController()->selectedTrack();

    selectionController()->dataSelectedOnTracksChanged().onReceive(this, [this](const std::vector<trackedit::TrackId>& tracks) {
        setDataSelectedTracks(tracks);
    });

    selectionController()->trackSelected().onReceive(this, [this](const trackedit::TrackId& trackId) {
        setSelectedTrack(trackId);
    });

    m_trackList.onChanged(this, [this]() {
        muse::async::Async::call(this, [this]() {
            load();
        });
    });

    m_trackList.onItemAdded(this, [this](const trackedit::Track& track) {
        beginInsertRows(QModelIndex(), m_trackList.size(), m_trackList.size());
        m_trackList.push_back(track);
        endInsertRows();

        subscribeOnTrackHeightChanges(track.id);
        updateTotalTracksHeight();
    });

    m_trackList.onItemRemoved(this, [this](const trackedit::Track& track) {
        for (size_t i = 0; i < m_trackList.size(); ++i) {
            if (m_trackList.at(i).id == track.id) {
                beginRemoveRows(QModelIndex(), i, i);
                m_trackList.erase(m_trackList.begin() + i);
                endRemoveRows();
                break;
            }
        }

        unsubscribeFromTrackHeightChanges(track.id);
        updateTotalTracksHeight();
    });

    m_trackList.onItemChanged(this, [this](const trackedit::Track& track) {
        for (size_t i = 0; i < m_trackList.size(); ++i) {
            if (m_trackList.at(i).id == track.id) {
                m_trackList[i] = track;

                emit dataChanged(index(i), index(i));
                break;
            }
        }
    });

    endResetModel();

    for (const trackedit::Track& track : m_trackList) {
        subscribeOnTrackHeightChanges(track.id);
    }

    updateTotalTracksHeight();
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

    const au::trackedit::Track& track = m_trackList.at(index.row());
    switch (role) {
    case TrackIdRole:
        return QVariant::fromValue(track.id);
    case IsDataSelectedRole: {
        return muse::contains(m_dataSelectedTracks, track.id);
    }
    case IsTrackSelectedRole: {
        return m_selectedTrack == track.id;
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
        { IsDataSelectedRole, "isDataSelected" },
        { IsTrackSelectedRole, "isTrackSelected" }
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

void TracksListClipsModel::setDataSelectedTracks(const std::vector<trackedit::TrackId>& tracks)
{
    if (m_dataSelectedTracks == tracks) {
        return;
    }
    m_dataSelectedTracks = tracks;
    emit dataSelectedTracksChanged();
    emit dataChanged(index(0), index(m_trackList.size() - 1), { IsDataSelectedRole });
}

void TracksListClipsModel::setSelectedTrack(const trackedit::TrackId trackId)
{
    if (m_selectedTrack == trackId) {
        return;
    }
    m_selectedTrack = trackId;
    emit selectedTrackChanged();
    emit dataChanged(index(0), index(m_trackList.size() - 1), { IsTrackSelectedRole });
}

void TracksListClipsModel::updateTotalTracksHeight()
{
    project::IAudacityProjectPtr prj = globalContext()->currentProject();
    IProjectViewStatePtr viewState = prj ? prj->viewState() : nullptr;
    if (!viewState) {
        return;
    }

    int height = 0;

    for (const trackedit::Track& track : m_trackList) {
        height += viewState->trackHeight(track.id).val;
    }

    setTotalTracksHeight(height);
}

void TracksListClipsModel::subscribeOnTrackHeightChanges(const trackedit::TrackId trackId)
{
    project::IAudacityProjectPtr prj = globalContext()->currentProject();
    IProjectViewStatePtr viewState = prj ? prj->viewState() : nullptr;

    muse::ValCh<int> trackHeightCh = viewState->trackHeight(trackId);
    trackHeightCh.ch.onReceive(this, [this](int) {
        updateTotalTracksHeight();
    });
}

void TracksListClipsModel::unsubscribeFromTrackHeightChanges(const trackedit::TrackId trackId)
{
    project::IAudacityProjectPtr prj = globalContext()->currentProject();
    IProjectViewStatePtr viewState = prj ? prj->viewState() : nullptr;
    viewState->trackHeight(trackId).ch.resetOnReceive(this);
}

int TracksListClipsModel::totalTracksHeight() const
{
    return m_totalTracksHeight;
}

void TracksListClipsModel::setTotalTracksHeight(int height)
{
    if (m_totalTracksHeight == height) {
        return;
    }

    m_totalTracksHeight = height;
    emit totalTracksHeightChanged();
}
