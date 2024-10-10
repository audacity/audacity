#include "trackslistmodel.h"

#include "global/async/async.h"
#include "global/containers.h"

#include "uicomponents/view/itemmultiselectionmodel.h"

#include "log.h"

using namespace au::projectscene;
using namespace au::project;
using namespace au::trackedit;

TracksListModel::TracksListModel(QObject* parent)
    : QAbstractListModel(parent)
{
    m_selectionModel = new muse::uicomponents::ItemMultiSelectionModel(this);
    m_selectionModel->setAllowedModifiers(Qt::ShiftModifier | Qt::ControlModifier);

    connect(m_selectionModel, &muse::uicomponents::ItemMultiSelectionModel::selectionChanged,
            [this](const QItemSelection& selected, const QItemSelection& deselected) {
        setItemsSelected(deselected.indexes(), false);
        setItemsSelected(selected.indexes(), true);

        updateRearrangementAvailability();
        updateRemovingAvailability();
    });

    onSelectedTracks(selectionController()->selectedTracks());

    selectionController()->tracksSelected().onReceive(this, [this](trackedit::TrackIdList tracksIds) {
        onSelectedTracks(tracksIds);
    });

    selectionController()->dataSelectedOnTracksSelected().onReceive(this, [this](trackedit::TrackIdList tracksIds) {
        if (!tracksIds.empty()) {
            m_audioDataSelected = true;
        }
    });

    connect(this, &TracksListModel::rowsInserted, this, [this]() {
        updateRemovingAvailability();
    });

    onProjectChanged();
    globalContext()->currentTrackeditProjectChanged().onNotify(this, [this]() {
        onProjectChanged();
    });
}

TracksListModel::~TracksListModel()
{
    deleteItems();
}

void TracksListModel::load()
{
    if (m_isLoadingBlocked) {
        return;
    }

    TRACEFUNC;

    beginResetModel();
    deleteItems();

    ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return;
    }

    muse::async::NotifyList<Track> tracks = prj->trackList();

    for (const Track& track : tracks) {
        m_trackList.push_back(buildTrackItem(track));
    }

    auto updateTrackItem = [this](const Track& track) {
        auto trackItem = findTrackItem(track.id);
        if (!trackItem) {
            return;
        }

        trackItem->init(track);
        updateRemovingAvailability();
    };

    tracks.onChanged(this, [this]() {
        muse::async::Async::call(this, [this]() {
            load();
        });
    });

    tracks.onItemAdded(this, [this, updateTrackItem](const Track& track) {
        beginInsertRows(QModelIndex(), m_trackList.size(), m_trackList.size());
        m_trackList.push_back(buildTrackItem(track));
        updateTrackItem(track);
        endInsertRows();
    });

    tracks.onItemRemoved(this, [this](const Track& track) {
        for (int i = 0; i < m_trackList.size(); ++i) {
            if (m_trackList.at(i)->trackId() == track.id) {
                beginRemoveRows(QModelIndex(), i, i);
                m_trackList.erase(m_trackList.begin() + i);
                endRemoveRows();
                break;
            }
        }
    });

    tracks.onItemChanged(this, [updateTrackItem](const Track& track) {
        updateTrackItem(track);
    });

    endResetModel();

    listenTracksSelectionChanged();

    emit isEmptyChanged();
    emit isAddingAvailableChanged(true);
}

void TracksListModel::addTrack(TrackTypes::Type type)
{
    if (type == TrackTypes::Type::MONO) {
        dispatcher()->dispatch("new-mono-track");
    } else if (type == TrackTypes::Type::STEREO) {
        dispatcher()->dispatch("new-stereo-track");
    } else if (type == TrackTypes::Type::LABEL) {
        dispatcher()->dispatch("new-label-track");
    }
}

void TracksListModel::addTracks(TrackTypes::Type type, int quantity)
{
    for (int i = 0; i < quantity; ++i) {
        addTrack(type);
    }
}

void TracksListModel::duplicateTrack(int index)
{
    UNUSED(index);
    NOT_IMPLEMENTED;
}

void TracksListModel::deleteTrack(int index)
{
    UNUSED(index);
    NOT_IMPLEMENTED;
}

void TracksListModel::openEffectsForTrack(int index)
{
    UNUSED(index);
    NOT_IMPLEMENTED;
}

QItemSelectionModel* TracksListModel::selectionModel() const
{
    return m_selectionModel;
}

void TracksListModel::selectRow(int rowIndex)
{
    if (m_audioDataSelected) {
        selectionController()->resetSelectedTracks();
        clearSelection();
        m_audioDataSelected = false;
    }

    selectionController()->resetDataSelection();
    QModelIndex modelIndex = index(rowIndex);
    m_selectionModel->select(modelIndex);
}

void TracksListModel::clearSelection()
{
    m_selectionModel->clear();
}

void TracksListModel::moveSelectedRowsUp()
{
    if (!m_isMovingUpAvailable) {
        return;
    }

    QModelIndexList selectedIndexList = m_selectionModel->selectedIndexes();
    if (selectedIndexList.isEmpty()) {
        return;
    }

    std::sort(selectedIndexList.begin(), selectedIndexList.end(), [](QModelIndex f, QModelIndex s) -> bool {
        return f.row() < s.row();
    });

    const QModelIndex& sourceRowFirst = selectedIndexList.first();

    moveRows(sourceRowFirst.parent(), sourceRowFirst.row(), selectedIndexList.count(), sourceRowFirst.parent(), sourceRowFirst.row() - 1);
}

void TracksListModel::moveSelectedRowsDown()
{
    if (!m_isMovingDownAvailable) {
        return;
    }

    QModelIndexList selectedIndexList = m_selectionModel->selectedIndexes();
    if (selectedIndexList.isEmpty()) {
        return;
    }

    std::sort(selectedIndexList.begin(), selectedIndexList.end(), [](const QModelIndex& f, const QModelIndex& s) -> bool {
        return f.row() < s.row();
    });

    const QModelIndex& sourceRowFirst = selectedIndexList.first();
    const QModelIndex& sourceRowLast = selectedIndexList.last();

    moveRows(sourceRowFirst.parent(), sourceRowFirst.row(), selectedIndexList.count(), sourceRowFirst.parent(), sourceRowLast.row() + 1);
}

void TracksListModel::removeSelectedRows()
{
    if (!m_isRemovingAvailable) {
        return;
    }

    QModelIndexList selectedIndexList = m_selectionModel->selectedIndexes();
    if (selectedIndexList.empty()) {
        return;
    }

    QModelIndex firstIndex = *std::min_element(selectedIndexList.cbegin(), selectedIndexList.cend(),
                                               [](const QModelIndex& f, const QModelIndex& s) {
        return f.row() < s.row();
    });

    removeRows(firstIndex.row(), selectedIndexList.size(), firstIndex.parent());
}

bool TracksListModel::moveRows(const QModelIndex& sourceParent, int sourceRow, int count, const QModelIndex& destinationParent,
                               int destinationChild)
{
    setLoadingBlocked(true);

    int sourceFirstRow = sourceRow;
    int sourceLastRow = sourceRow + count - 1;
    int destinationRow = destinationChild + 1;

    NOT_IMPLEMENTED;
    // todo: move

    // if (m_dragInProgress) {
    //     m_activeDragMoveParams = sourceParentItem->buildMoveParams(sourceRow, count, destinationParentItem, destinationRow);
    // }

    // beginMoveRows(sourceParent, sourceFirstRow, sourceLastRow, destinationParent, destinationRow);
    // sourceParentItem->moveChildren(sourceFirstRow, count, destinationParentItem, destinationRow, !m_dragInProgress);
    // endMoveRows();

    updateRearrangementAvailability();

    setLoadingBlocked(false);

    return true;
}

void TracksListModel::startActiveDrag()
{
    m_dragInProgress = true;
}

void TracksListModel::endActiveDrag()
{
    setLoadingBlocked(true);

    NOT_IMPLEMENTED;
    // todo: move

    // m_activeDragMoveParams = MoveParams();
    m_dragInProgress = false;

    setLoadingBlocked(false);
}

void TracksListModel::clear()
{
    TRACEFUNC;

    beginResetModel();
    deleteItems();
    endResetModel();

    emit isEmptyChanged();
    emit isAddingAvailableChanged(false);
}

void TracksListModel::deleteItems()
{
    m_selectionModel->clear();

    for (TrackItem* trackItem : m_trackList) {
        trackItem->deleteLater();
    }

    m_trackList.clear();
}

void TracksListModel::setIsMovingUpAvailable(bool isMovingUpAvailable)
{
    if (m_isMovingUpAvailable == isMovingUpAvailable) {
        return;
    }

    m_isMovingUpAvailable = isMovingUpAvailable;
    emit isMovingUpAvailableChanged(m_isMovingUpAvailable);
}

void TracksListModel::setIsMovingDownAvailable(bool isMovingDownAvailable)
{
    if (m_isMovingDownAvailable == isMovingDownAvailable) {
        return;
    }

    m_isMovingDownAvailable = isMovingDownAvailable;
    emit isMovingDownAvailableChanged(m_isMovingDownAvailable);
}

void TracksListModel::setIsRemovingAvailable(bool isRemovingAvailable)
{
    if (m_isRemovingAvailable == isRemovingAvailable) {
        return;
    }

    m_isRemovingAvailable = isRemovingAvailable;
    emit isRemovingAvailableChanged(m_isRemovingAvailable);
}

void TracksListModel::setItemsSelected(const QModelIndexList& indexes, bool selected)
{
    trackedit::TrackIdList idsToModify;
    for (const QModelIndex& index : indexes) {
        if (TrackItem* item = modelIndexToItem(index)) {
            item->setIsSelected(selected);
            idsToModify.push_back(item->trackId());
        }
    }

    // keep selectionController in sync with muse::uicomponents::ItemMultiSelectionModel
    auto alreadySelectedTracksIds = selectionController()->selectedTracks();
    if (selected) {
        // if selecting new tracks, add them to the existing selection
        alreadySelectedTracksIds.insert(alreadySelectedTracksIds.end(), idsToModify.begin(), idsToModify.end());
    } else {
        // if deselecting tracks, remove them from the existing selection
        alreadySelectedTracksIds.erase(
            std::remove_if(alreadySelectedTracksIds.begin(), alreadySelectedTracksIds.end(),
                           [&idsToModify](const TrackId& trackId) {
                               return muse::contains(idsToModify, trackId);
                           }),
            alreadySelectedTracksIds.end());

    }
    selectionController()->setSelectedTracks(alreadySelectedTracksIds);
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
    if (!index.isValid() || index.row() >= rowCount() || role != rItemData) {
        return QVariant();
    }

    return QVariant::fromValue(m_trackList.at(index.row()));
}

int TracksListModel::rowCount(const QModelIndex& parent) const
{
    UNUSED(parent);
    return m_trackList.count();
}

bool TracksListModel::isMovingUpAvailable() const
{
    return m_isMovingUpAvailable;
}

bool TracksListModel::isMovingDownAvailable() const
{
    return m_isMovingDownAvailable;
}

bool TracksListModel::isRemovingAvailable() const
{
    return m_isRemovingAvailable;
}

bool TracksListModel::isAddingAvailable() const
{
    return !isEmpty();
}

bool TracksListModel::isEmpty() const
{
    return m_trackList.empty();
}

void TracksListModel::updateRearrangementAvailability()
{
    QModelIndexList selectedIndexList = m_selectionModel->selectedIndexes();

    if (selectedIndexList.isEmpty()) {
        updateMovingUpAvailability(false);
        updateMovingDownAvailability(false);
        return;
    }

    std::sort(selectedIndexList.begin(), selectedIndexList.end(), [](const QModelIndex& f, const QModelIndex& s) -> bool {
        return f.row() < s.row();
    });

    bool isRearrangementAvailable = true;

    QMutableListIterator<QModelIndex> it(selectedIndexList);

    while (it.hasNext() && selectedIndexList.count() > 1) {
        int nextRow = it.next().row();
        int previousRow = it.peekPrevious().row();

        isRearrangementAvailable = (nextRow - previousRow <= 1);

        if (!isRearrangementAvailable) {
            updateMovingUpAvailability(isRearrangementAvailable);
            updateMovingDownAvailability(isRearrangementAvailable);
            return;
        }
    }

    updateMovingUpAvailability(isRearrangementAvailable, selectedIndexList.first());
    updateMovingDownAvailability(isRearrangementAvailable, selectedIndexList.last());
}

void TracksListModel::updateMovingUpAvailability(bool isSelectionMovable, const QModelIndex& firstSelectedRowIndex)
{
    bool isRowInBoundaries = firstSelectedRowIndex.isValid() ? firstSelectedRowIndex.row() > 0 : false;

    setIsMovingUpAvailable(isSelectionMovable && isRowInBoundaries);
}

void TracksListModel::updateMovingDownAvailability(bool isSelectionMovable, const QModelIndex& lastSelectedRowIndex)
{
    int lastItemRowIndex = rowCount() - 1;
    bool isRowInBoundaries = lastSelectedRowIndex.isValid() ? lastSelectedRowIndex.row() < lastItemRowIndex : false;

    setIsMovingDownAvailable(isSelectionMovable && isRowInBoundaries);
}

void TracksListModel::updateRemovingAvailability()
{
    QModelIndexList selectedIndexes = m_selectionModel->selectedIndexes();
    setIsRemovingAvailable(!selectedIndexes.empty());
}

bool TracksListModel::removeRows(int row, int count, const QModelIndex& parent)
{
    if (!m_isRemovingAvailable) {
        return false;
    }

    setLoadingBlocked(true);
    beginRemoveRows(parent, row, row + count - 1);

    NOT_IMPLEMENTED;
    // todo: remove

    endRemoveRows();
    setLoadingBlocked(false);

    emit isEmptyChanged();

    return true;
}

void TracksListModel::onProjectChanged()
{
    if (m_isLoadingBlocked) {
        m_projectChangedWhileLoadingWasBlocked = true;
        return;
    }

    ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();

    if (prj) {
        load();
    } else {
        clear();
    }

    m_projectChangedWhileLoadingWasBlocked = false;
}

TrackItem* TracksListModel::buildTrackItem(const Track& track)
{
    TrackItem* item = new TrackItem(this);
    item->init(track);

    return item;
}

TrackItem* TracksListModel::findTrackItem(const trackedit::TrackId& trackId)
{
    for (TrackItem* track: m_trackList) {
        if (track->trackId() == trackId) {
            return track;
        }
    }

    return nullptr;
}

void TracksListModel::setLoadingBlocked(bool blocked)
{
    m_isLoadingBlocked = blocked;

    if (!m_isLoadingBlocked && m_projectChangedWhileLoadingWasBlocked) {
        onProjectChanged();
    }
}

void TracksListModel::listenTracksSelectionChanged()
{
    NOT_IMPLEMENTED;
}

void TracksListModel::updateSelectedRows()
{
    NOT_IMPLEMENTED;
}

TrackItem* TracksListModel::modelIndexToItem(const QModelIndex& index) const
{
    if (!index.isValid() || index.row() >= rowCount()) {
        return nullptr;
    }

    return m_trackList.at(index.row());
}

void TracksListModel::onSelectedTracks(const TrackIdList &tracksIds)
{
    for (TrackItem* item : m_trackList) {
        item->setIsSelected(muse::contains(tracksIds, item->trackId()));
    }
}
