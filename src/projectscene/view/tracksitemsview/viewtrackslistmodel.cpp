/*
* Audacity: A Digital Audio Editor
*/
#include "viewtrackslistmodel.h"

#include "global/async/async.h"

#include "global/containers.h"
#include "playback/playbacktypes.h"
#include "trackedit/dom/track.h"

using namespace au::projectscene;

namespace {
constexpr int numDecimals(float value)
{
    constexpr int MAX_DECIMAL_PLACES = 4;
    constexpr float POWER_OF_TEN = 10000.0f;

    int rounded_scaled_value = static_cast<int>((value * POWER_OF_TEN) + 0.5f);
    int integer_part = static_cast<int>(value);
    int decimal_part_scaled = rounded_scaled_value - (integer_part * static_cast<int>(POWER_OF_TEN));

    if (decimal_part_scaled == 0) {
        return 0;
    }

    int count = MAX_DECIMAL_PLACES;
    int power_of_ten = 10;

    while (count > 0 && (decimal_part_scaled % power_of_ten) == 0) {
        count--;
        power_of_ten *= 10;
    }

    return count;
}

static_assert(numDecimals(0.14999999) == 2);
static_assert(numDecimals(1.00) == 0);
static_assert(numDecimals(0.005) == 3);
}

ViewTracksListModel::ViewTracksListModel(QObject* parent)
    : QAbstractListModel(parent)
{
}

void ViewTracksListModel::load()
{
    globalContext()->currentTrackeditProjectChanged().onNotify(this, [this]() {
        load();
    }, muse::async::Asyncable::Mode::SetReplace);

    au::trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return;
    }

    trackeditInteraction()->cancelDragEditRequested().onNotify(this, [this]() {
        emit escapePressed();
    }, muse::async::Asyncable::Mode::SetReplace);

    trackPlaybackControl()->muteOrSoloChanged().onReceive(this, [this] (long) {
        emit dataChanged(index(0), index(static_cast<int>(m_trackList.size()) - 1), { IsTrackAudibleRole });
    }, muse::async::Asyncable::Mode::SetReplace);

    projectHistory()->historyChanged().onNotify(this, [this]() {
        emit dataChanged(index(0), index(m_trackList.size() - 1), { IsTrackAudibleRole });
    }, muse::async::Asyncable::Mode::SetReplace);

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
    }, muse::async::Asyncable::Mode::SetReplace);

    selectionController()->focusedTrackChanged().onReceive(this, [this](const trackedit::TrackId& trackId) {
        Q_UNUSED(trackId);
        if (m_trackList.empty()) {
            return;
        }

        QModelIndex beginIndex = index(0);
        QModelIndex lastIndex = index(static_cast<int>(m_trackList.size()) - 1);

        emit dataChanged(beginIndex, lastIndex, { IsTrackFocusedRole });
    }, muse::async::Asyncable::Mode::SetReplace);

    selectionController()->clipsSelected().onReceive(this, [this](const trackedit::ClipKeyList& clipKeys) {
        Q_UNUSED(clipKeys);

        const int lastIndex = static_cast<int>(m_trackList.size()) - 1;
        emit dataChanged(index(0), index(lastIndex), { IsMultiSelectionActiveRole });
    }, muse::async::Asyncable::Mode::SetReplace);

    selectionController()->dataSelectedStartTimeChanged().onReceive(this, [this](trackedit::secs_t begin) {
        Q_UNUSED(begin);
        if (m_trackList.empty()) {
            return;
        }

        const int lastIndex = static_cast<int>(m_trackList.size()) - 1;
        emit dataChanged(index(0), index(lastIndex), { IsDataSelectedRole });
    }, muse::async::Asyncable::Mode::SetReplace);

    selectionController()->dataSelectedEndTimeChanged().onReceive(this, [this](trackedit::secs_t end) {
        Q_UNUSED(end);
        if (m_trackList.empty()) {
            return;
        }

        const int lastIndex = static_cast<int>(m_trackList.size()) - 1;
        emit dataChanged(index(0), index(lastIndex), { IsDataSelectedRole });
    }, muse::async::Asyncable::Mode::SetReplace);

    prj->tracksChanged().onReceive(this, [this](const std::vector<au::trackedit::Track> tracks) {
        Q_UNUSED(tracks);
        muse::async::Async::call(this, [this]() {
            load();
        });
    }, muse::async::Asyncable::Mode::SetReplace);

    prj->trackAdded().onReceive(this, [this](const trackedit::Track& track) {
        const int size = static_cast<int>(m_trackList.size());
        beginInsertRows(QModelIndex(), size, size);
        m_trackList.push_back(track);
        endInsertRows();
    }, muse::async::Asyncable::Mode::SetReplace);

    prj->trackInserted().onReceive(this, [this](const trackedit::Track& track, const int pos) {
        const int size = static_cast<int>(m_trackList.size());
        const int index = ((pos >= 0) && (pos <= size)) ? pos : size;

        beginInsertRows(QModelIndex(), index, index);
        m_trackList.insert(m_trackList.begin() + index, track);

        endInsertRows();
    }, muse::async::Asyncable::Mode::SetReplace);

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
    }, muse::async::Asyncable::Mode::SetReplace);

    prj->trackRemoved().onReceive(this, [this](const trackedit::Track& track) {
        for (size_t i = 0; i < m_trackList.size(); ++i) {
            if (m_trackList.at(i).id == track.id) {
                beginRemoveRows(QModelIndex(), i, i);
                m_trackList.erase(m_trackList.begin() + i);
                endRemoveRows();
                break;
            }
        }
    }, muse::async::Asyncable::Mode::SetReplace);

    prj->trackChanged().onReceive(this, [this](const trackedit::Track& track) {
        for (size_t i = 0; i < m_trackList.size(); ++i) {
            if (m_trackList.at(i).id == track.id) {
                m_trackList[i] = track;

                emit dataChanged(index(i), index(i));
                break;
            }
        }
        emit verticalRulerWidthChanged();
    }, muse::async::Asyncable::Mode::SetReplace);

    endResetModel();

    projectSceneConfiguration()->isVerticalRulersVisibleChanged().onReceive(this, [this](bool) {
        emit isVerticalRulersVisibleChanged();
    }, muse::async::Asyncable::Mode::SetReplace);
    emit isVerticalRulersVisibleChanged();

    playbackConfiguration()->playbackMeterDbRangeChanged().onNotify(this, [this]() {
        emit dataChanged(index(0), index(m_trackList.size() - 1), { DbRangeRole });
    }, muse::async::Asyncable::Mode::SetReplace);

    const IProjectViewStatePtr viewState = globalContext()->currentProject()->viewState();

    if (!viewState) {
        return;
    }

    viewState->totalTrackHeight().ch.onReceive(this, [this](int) {
        emit totalTracksHeightChanged();
    }, muse::async::Asyncable::Mode::SetReplace);

    viewState->verticalRulerWidth().ch.onReceive(this, [this](int) {
        emit verticalRulerWidthChanged();
    }, muse::async::Asyncable::Mode::SetReplace);
}

void ViewTracksListModel::handleDroppedFiles(const QStringList& fileUrls)
{
    std::vector<muse::io::path_t> localPaths;
    for (const auto& fileUrl : fileUrls) {
        QUrl url(fileUrl);
        localPaths.push_back(muse::io::path_t(url.toLocalFile()));
    }

    project::IAudacityProjectPtr prj = globalContext()->currentProject();
    prj->import(localPaths);
}

int ViewTracksListModel::rowCount(const QModelIndex&) const
{
    return static_cast<int>(m_trackList.size());
}

QVariant ViewTracksListModel::data(const QModelIndex& index, int role) const
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
        if (track.type == au::trackedit::TrackType::Label) {
            return false;
        }

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
    case TypeRole:
        return QVariant::fromValue(track.type);

    case IsStereoRole:
        return track.type == au::trackedit::TrackType::Stereo;

    case IsWaveformViewVisibleRole:
        return track.viewType == au::trackedit::TrackViewType::Waveform
               || track.viewType == au::trackedit::TrackViewType::WaveformAndSpectrogram;

    case IsSpectrogramViewVisibleRole:
        return track.viewType == au::trackedit::TrackViewType::Spectrogram
               || track.viewType == au::trackedit::TrackViewType::WaveformAndSpectrogram;

    case DbRangeRole:
        return playback::PlaybackMeterDbRange::toDouble(playbackConfiguration()->playbackMeterDbRange());

    default:
        break;
    }

    return {};
}

QHash<int, QByteArray> ViewTracksListModel::roleNames() const
{
    static QHash<int, QByteArray> roles
    {
        { TypeRole, "trackType" },
        { TrackIdRole, "trackId" },
        { IsDataSelectedRole, "isDataSelected" },
        { IsTrackSelectedRole, "isTrackSelected" },
        { IsTrackFocusedRole, "isTrackFocused" },
        { IsMultiSelectionActiveRole, "isMultiSelectionActive" },
        { IsTrackAudibleRole, "isTrackAudible" },
        { IsStereoRole, "isStereo" },
        { DbRangeRole, "dbRange" },
        { IsWaveformViewVisibleRole, "isWaveformViewVisible" },
        { IsSpectrogramViewVisibleRole, "isSpectrogramViewVisible" }
    };
    return roles;
}

bool ViewTracksListModel::isVerticalRulersVisible() const
{
    return projectSceneConfiguration()->isVerticalRulersVisible();
}

int ViewTracksListModel::verticalRulerWidth() const
{
    const project::IAudacityProjectPtr prj = globalContext()->currentProject();
    const IProjectViewStatePtr viewState = prj ? prj->viewState() : nullptr;

    if (!viewState) {
        return 0;
    }

    return viewState->verticalRulerWidth().val;
}

int ViewTracksListModel::totalTracksHeight() const
{
    const project::IAudacityProjectPtr prj = globalContext()->currentProject();
    const IProjectViewStatePtr viewState = prj ? prj->viewState() : nullptr;

    if (!viewState) {
        return 0;
    }

    return viewState->totalTrackHeight().val;
}
