#include "TrackListModel.h"

#include "Track.h"
#include "TrackAdapter.h"
#include "WaveTrack.h"

TracksListClipsModel::TracksListClipsModel(QObject* parent)
    : QAbstractListModel(parent)
{
}

void TracksListClipsModel::load()
{
    au::project::IAudacityProjectPtr prj = globalContext()->currentProject();
    if (!prj) {
        return;
    }

    beginResetModel();

    AudacityProject* project = reinterpret_cast<AudacityProject*>(prj->au3ProjectPtr());

    mTrackList = TrackList::Get(*project).shared_from_this();

    mTrackListSubscription = mTrackList->Subscribe(*this, &TracksListClipsModel::OnTrackListEvent);

    for (auto track : mTrackList->Any<>()) {
        auto adapter = track->TypeSwitch<TrackAdapterBase*>([this](auto& track)
        {
            return TrackAdapterBase::Create(track, this);
        });
        mTrackAdapters.push_back(adapter);
    }

    endResetModel();
}

int TracksListClipsModel::rowCount(const QModelIndex& parent) const
{
    return static_cast<int>(mTrackAdapters.size());
}

QVariant TracksListClipsModel::data(const QModelIndex& index, int role) const
{
    if (index.row() >= 0 && index.row() < mTrackAdapters.size()) {
        auto adapter = mTrackAdapters[index.row()];
        switch (role) {
        case TypeRole:
            return adapter->GetType();
        case TrackRole:
            return QVariant::fromValue(adapter);
        default: break;
        }
    }
    return {};
}

QHash<int, QByteArray> TracksListClipsModel::roleNames() const
{
    static QHash<int, QByteArray> mapping
    {
        { TypeRole, "typeData" },
        { TrackRole, "trackData" }
    };
    return mapping;
}

void TracksListClipsModel::move(int from, int to)
{
    auto track = mTrackAdapters[from]->GetTrack();

    if (to > from) {
        const auto distance = to - from;
        for (int i = 0; i < distance; ++i) {
            mTrackList->MoveDown(*track);
        }
    } else if (to < from) {
        const auto distance = from - to;
        for (int i = 0; i < distance; ++i) {
            mTrackList->MoveUp(*track);
        }
    }
}

void TracksListClipsModel::OnTrackListEvent(TrackListEvent event)
{
    switch (event.mType) {
    case TrackListEvent::SELECTION_CHANGE:
        break;
    case TrackListEvent::TRACK_DATA_CHANGE:
        break;
    case TrackListEvent::PERMUTED:
    {
        if (const auto track = event.mpTrack.lock()) {
            int index = 0;
            for (auto it = mTrackAdapters.begin(); it < mTrackAdapters.end(); ++it) {
                if ((*it)->GetTrack() == track.get()) {
                    beginMoveRows(QModelIndex(), index, index, QModelIndex(), index - 1);
                    std::swap(*it, *std::prev(it));
                    endMoveRows();
                    break;
                }
                ++index;
            }
        }
    } break;
    case TrackListEvent::RESIZING:
    {
        if (const auto track = event.mpTrack.lock()) {
            if (track->IsLeader()) {
                HandleTrackAdded(*track);
            } else {
                HandleTrackRemoved(*track);
            }
        }
    }
    break;
    case TrackListEvent::ADDITION:
    {
        if (const auto track = event.mpTrack.lock()) {
            HandleTrackAdded(*track);
        }
    } break;
    case TrackListEvent::DELETION:
    {
        if (const auto track = event.mpTrack.lock()) {
            HandleTrackRemoved(*track);
        }
    } break;
    default: break;
    }
}

void TracksListClipsModel::HandleTrackAdded(Track& track)
{
    auto it = mTrackList->Find(&track);
    if (it == mTrackList->end()) {
        return;
    }

    for (const auto adapter : mTrackAdapters) {
        if (adapter->GetTrack() == &track) {
            return;
        }
    }

    const auto adapter = track.TypeSwitch<TrackAdapterBase*>([this](auto& track)
    {
        return TrackAdapterBase::Create(track, this);
    });
    const auto row = std::distance(mTrackList->begin(), it);
    beginInsertRows(QModelIndex(), row, row);
    mTrackAdapters.insert(mTrackAdapters.begin() + row, adapter);
    endInsertRows();
}

void TracksListClipsModel::HandleTrackRemoved(Track& track)
{
    int row = 0;
    for (auto adapterIt = mTrackAdapters.begin(); adapterIt != mTrackAdapters.end(); ++adapterIt) {
        if (&track == (*adapterIt)->GetTrack()) {
            (*adapterIt)->deleteLater();
            beginRemoveRows(QModelIndex(), row, row);
            mTrackAdapters.erase(adapterIt);
            endRemoveRows();
            break;
        }
        ++row;
    }
}
