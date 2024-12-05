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
    configuration()->isVerticalRulersVisibleChanged().onReceive(this, [this](bool isVerticalRulersVisible) {
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

    selectionController()->tracksSelected().onReceive(this, [this](const trackedit::TrackIdList& tracksIds) {
        Q_UNUSED(tracksIds);
        if (m_trackList.empty()) {
            return;
        }
        emit dataChanged(index(0), index(m_trackList.size() - 1), { IsTrackSelectedRole });
        emit dataChanged(index(0), index(m_trackList.size() - 1), { IsDataSelectedRole });
    });

    selectionController()->clipsSelected().onReceive(this, [this](const trackedit::ClipKeyList& clipKeys) {
        Q_UNUSED(clipKeys);
        emit dataChanged(index(0), index(m_trackList.size() - 1), { IsMultiSelectionActiveRole });
    });

    selectionController()->dataSelectedStartTimeChanged().onReceive(this, [this](trackedit::secs_t begin) {
        Q_UNUSED(begin);
        if (m_trackList.empty()) {
            return;
        }
        emit dataChanged(index(0), index(m_trackList.size() - 1), { IsDataSelectedRole });
    });

    selectionController()->dataSelectedEndTimeChanged().onReceive(this, [this](trackedit::secs_t end) {
        Q_UNUSED(end);
        if (m_trackList.empty()) {
            return;
        }
        emit dataChanged(index(0), index(m_trackList.size() - 1), { IsDataSelectedRole });
    });

    prj->tracksChanged().onReceive(this, [this](const std::vector<au::trackedit::Track> tracks) {
        Q_UNUSED(tracks);
        muse::async::Async::call(this, [this]() {
            load();
        });
    });

    prj->trackAdded().onReceive(this, [this](const trackedit::Track& track) {
        beginInsertRows(QModelIndex(), m_trackList.size(), m_trackList.size());
        m_trackList.push_back(track);
        endInsertRows();

        subscribeOnTrackHeightChanges(track.id);
        updateTotalTracksHeight();
    });

    prj->trackInserted().onReceive(this, [this](const trackedit::Track& track, const int pos) {
        int index = pos >= 0 && pos <= static_cast<int>(m_trackList.size()) ? pos : m_trackList.size();

        beginInsertRows(QModelIndex(), index, index);
        m_trackList.insert(m_trackList.begin() + index, track);

        endInsertRows();

        subscribeOnTrackHeightChanges(track.id);
        updateTotalTracksHeight();
    });

    prj->trackMoved().onReceive(this, [this](const trackedit::Track& track, const int pos) {
        auto it = std::find_if(m_trackList.begin(), m_trackList.end(), [&track](const trackedit::Track& it) { return it.id == track.id; });

        if (it == m_trackList.end()) {
            return;
        }

        int from = std::distance(m_trackList.begin(), it);
        int to = std::clamp(pos, 0, static_cast<int>(m_trackList.size()));

        beginMoveRows(QModelIndex(), from, from, QModelIndex(), to > from ? to + 1 : to);
        m_trackList.erase(it);
        m_trackList.insert(m_trackList.begin() + to, track);
        endMoveRows();

        subscribeOnTrackHeightChanges(track.id);
        updateTotalTracksHeight();
    });

    prj->trackRemoved().onReceive(this, [this](const trackedit::Track& track) {
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

    prj->trackChanged().onReceive(this, [this](const trackedit::Track& track) {
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
        return muse::contains(selectionController()->selectedTracks(), track.id) && selectionController()->timeSelectionIsNotEmpty();
    }
    case IsTrackSelectedRole: {
        return muse::contains(selectionController()->selectedTracks(), track.id);
    }
    case IsMultiSelectionActiveRole: {
        return selectionController()->selectedClips().size() > 1;
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
        { IsTrackSelectedRole, "isTrackSelected" },
        { IsMultiSelectionActiveRole, "isMultiSelectionActive" }
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
