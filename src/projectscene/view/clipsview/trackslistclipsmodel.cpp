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

    dispatcher()->reg(this, "split-tool", this, &TracksListClipsModel::toggleSplitTool);
}

void TracksListClipsModel::load()
{
    au::trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return;
    }

    setIsVerticalRulersVisible(configuration()->isVerticalRulersVisible());

    trackPlaybackControl()->muteOrSoloChanged().onReceive(this, [this] (long) {
        emit dataChanged(index(0), index(m_trackList.size() - 1), { IsTrackAudibleRole });
    });

    beginResetModel();

    m_trackList = prj->trackList();

    selectionController()->tracksSelected().onReceive(this, [this](const trackedit::TrackIdList& tracksIds) {
        Q_UNUSED(tracksIds);
        if (m_trackList.empty()) {
            return;
        }

        QModelIndex beginIndex = index(0);
        QModelIndex lastIndex = index(static_cast<int>(m_trackList.size()) - 1);

        emit dataChanged(beginIndex, lastIndex, { IsTrackSelectedRole });
        emit dataChanged(beginIndex, lastIndex, { IsDataSelectedRole });
    });

    selectionController()->focusedTrackChanged().onReceive(this, [this](const trackedit::TrackId& trackId) {
        Q_UNUSED(trackId);
        if (m_trackList.empty()) {
            return;
        }

        QModelIndex beginIndex = index(0);
        QModelIndex lastIndex = index(static_cast<int>(m_trackList.size()) - 1);

        emit dataChanged(beginIndex, lastIndex, { IsTrackFocusedRole });
    });

    selectionController()->clipsSelected().onReceive(this, [this](const trackedit::ClipKeyList& clipKeys) {
        Q_UNUSED(clipKeys);

        const int lastIndex = static_cast<int>(m_trackList.size()) - 1;
        emit dataChanged(index(0), index(lastIndex), { IsMultiSelectionActiveRole });
    });

    selectionController()->dataSelectedStartTimeChanged().onReceive(this, [this](trackedit::secs_t begin) {
        Q_UNUSED(begin);
        if (m_trackList.empty()) {
            return;
        }

        const int lastIndex = static_cast<int>(m_trackList.size()) - 1;
        emit dataChanged(index(0), index(lastIndex), { IsDataSelectedRole });
    });

    selectionController()->dataSelectedEndTimeChanged().onReceive(this, [this](trackedit::secs_t end) {
        Q_UNUSED(end);
        if (m_trackList.empty()) {
            return;
        }

        const int lastIndex = static_cast<int>(m_trackList.size()) - 1;
        emit dataChanged(index(0), index(lastIndex), { IsDataSelectedRole });
    });

    prj->tracksChanged().onReceive(this, [this](const std::vector<au::trackedit::Track> tracks) {
        Q_UNUSED(tracks);
        muse::async::Async::call(this, [this]() {
            load();
        });
    });

    prj->trackAdded().onReceive(this, [this](const trackedit::Track& track) {
        const int size = static_cast<int>(m_trackList.size());
        beginInsertRows(QModelIndex(), size, size);
        m_trackList.push_back(track);
        endInsertRows();
    });

    prj->trackInserted().onReceive(this, [this](const trackedit::Track& track, const int pos) {
        const int size = static_cast<int>(m_trackList.size());
        const int index = ((pos >= 0) && (pos <= size)) ? pos : size;

        beginInsertRows(QModelIndex(), index, index);
        m_trackList.insert(m_trackList.begin() + index, track);

        endInsertRows();
    });

    prj->trackMoved().onReceive(this, [this](const trackedit::Track& track, const int pos) {
        const auto iterator = std::find_if(m_trackList.begin(), m_trackList.end(), [&track](const trackedit::Track& it)
        {
            return it.id == track.id;
        });

        if (iterator == m_trackList.end()) {
            return;
        }

        const int from = static_cast<int>(std::distance(m_trackList.begin(), iterator));
        const int to = std::clamp(pos, 0, static_cast<int>(m_trackList.size()));

        beginMoveRows(QModelIndex(), from, from, QModelIndex(), to > from ? to + 1 : to);
        m_trackList.erase(iterator);
        m_trackList.insert(m_trackList.begin() + to, track);
        endMoveRows();
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

    const IProjectViewStatePtr viewState = globalContext()->currentProject()->viewState();

    if (!viewState) {
        return;
    }

    viewState->totalTrackHeight().ch.onReceive(this, [this](int value) {
        emit totalTracksHeightChanged();
    });
}

void TracksListClipsModel::startUserInteraction()
{
    projectHistory()->startUserInteraction();
}

void TracksListClipsModel::endUserInteraction()
{
    projectHistory()->endUserInteraction();
}

void TracksListClipsModel::handleDroppedFiles(const QStringList& fileUrls)
{
    std::vector<muse::io::path_t> localPaths;
    for (const auto& fileUrl : fileUrls) {
        QUrl url(fileUrl);
        localPaths.push_back(muse::io::path_t(url.toLocalFile()));
    }

    project::IAudacityProjectPtr prj = globalContext()->currentProject();
    prj->import(localPaths);
}

void TracksListClipsModel::splitAt(trackedit::TrackId id, double t)
{
    if (id < 0) {
        return;
    }
    std::vector<muse::secs_t> pivots { t };

    LOGD() << "Splitting track at " << t;
    dispatcher()->dispatch("track-split-at",
                           muse::actions::ActionData::make_arg2<trackedit::TrackIdList, std::vector<muse::secs_t> >({ id },
                                                                                                                    pivots));
}

int TracksListClipsModel::rowCount(const QModelIndex&) const
{
    return static_cast<int>(m_trackList.size());
}

QVariant TracksListClipsModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid()) {
        return {};
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
    case IsTrackFocusedRole: {
        return selectionController()->focusedTrack() == track.id;
    }
    case IsMultiSelectionActiveRole: {
        return selectionController()->selectedClips().size() > 1;
    }
    case IsTrackAudibleRole: {
        if (trackPlaybackControl()->muted(track.id)) {
            assert(!trackPlaybackControl()->solo(track.id));
            return false;
        } else if (trackPlaybackControl()->solo(track.id)) {
            return true;
        } else {
            return std::none_of(m_trackList.begin(), m_trackList.end(), [this](const au::trackedit::Track& t) {
                    return trackPlaybackControl()->solo(t.id);
                });
        }
    }
    default:
        break;
    }

    return {};
}

QHash<int, QByteArray> TracksListClipsModel::roleNames() const
{
    static QHash<int, QByteArray> roles
    {
        //{ TypeRole, "trackType" },
        { TrackIdRole, "trackId" },
        { IsDataSelectedRole, "isDataSelected" },
        { IsTrackSelectedRole, "isTrackSelected" },
        { IsTrackFocusedRole, "isTrackFocused" },
        { IsMultiSelectionActiveRole, "isMultiSelectionActive" },
        { IsTrackAudibleRole, "isTrackAudible" },
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

void TracksListClipsModel::toggleSplitTool()
{
    setIsSplitMode(!isSplitMode());
}

bool TracksListClipsModel::isSplitMode() const
{
    return m_isSplitMode;
}

void TracksListClipsModel::setIsSplitMode(bool newIsSplitMode)
{
    if (m_isSplitMode == newIsSplitMode) {
        return;
    }
    m_isSplitMode = newIsSplitMode;
    emit isSplitModeChanged();
}

int TracksListClipsModel::totalTracksHeight() const
{
    const project::IAudacityProjectPtr prj = globalContext()->currentProject();
    const IProjectViewStatePtr viewState = prj ? prj->viewState() : nullptr;

    if (!viewState) {
        return 0;
    }

    return viewState->totalTrackHeight().val;
}
