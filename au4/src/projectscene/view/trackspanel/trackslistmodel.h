#ifndef AU_PROJECTSCENE_TRACKSLISTMODEL_H
#define AU_PROJECTSCENE_TRACKSLISTMODEL_H

#include <QAbstractListModel>

#include "global/async/asyncable.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "types/tracktypes.h"

#include "trackitem.h"

namespace muse::uicomponents {
class ItemMultiSelectionModel;
}

class QItemSelectionModel;

namespace au::projectscene {
class TracksListModel : public QAbstractListModel, public muse::async::Asyncable
{
    Q_OBJECT

    muse::Inject<au::context::IGlobalContext> globalContext;

    Q_PROPERTY(bool isMovingUpAvailable READ isMovingUpAvailable NOTIFY isMovingUpAvailableChanged)
    Q_PROPERTY(bool isMovingDownAvailable READ isMovingDownAvailable NOTIFY isMovingDownAvailableChanged)
    Q_PROPERTY(bool isRemovingAvailable READ isRemovingAvailable NOTIFY isRemovingAvailableChanged)
    Q_PROPERTY(bool isAddingAvailable READ isAddingAvailable NOTIFY isAddingAvailableChanged)
    Q_PROPERTY(bool isEmpty READ isEmpty NOTIFY isEmptyChanged)

public:
    TracksListModel(QObject* parent = nullptr);
    ~TracksListModel() override;

    Q_INVOKABLE void load();

    Q_INVOKABLE void addTrack();
    Q_INVOKABLE void addTracks(TrackTypes::Type type, int quantity);
    Q_INVOKABLE void duplicateTrack(int index);
    Q_INVOKABLE void deleteTrack(int index);
    Q_INVOKABLE void openEffectsForTrack(int index);

    Q_INVOKABLE QItemSelectionModel* selectionModel() const;
    Q_INVOKABLE void selectRow(int rowIndex);
    Q_INVOKABLE void clearSelection();
    Q_INVOKABLE void moveSelectedRowsUp();
    Q_INVOKABLE void moveSelectedRowsDown();
    Q_INVOKABLE void removeSelectedRows();

    Q_INVOKABLE bool moveRows(const QModelIndex& sourceParent, int sourceRow, int count, const QModelIndex& destinationParent,
                              int destinationChild) override;

    Q_INVOKABLE void startActiveDrag();
    Q_INVOKABLE void endActiveDrag();

    QHash<int, QByteArray> roleNames() const override;
    QVariant data(const QModelIndex& index, int role) const override;
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;

    bool isMovingUpAvailable() const;
    bool isMovingDownAvailable() const;
    bool isRemovingAvailable() const;
    bool isAddingAvailable() const;
    bool isEmpty() const;

signals:
    void isMovingUpAvailableChanged(bool isMovingUpAvailable);
    void isMovingDownAvailableChanged(bool isMovingDownAvailable);
    void isAddingAvailableChanged(bool isAddingAvailable);
    void isRemovingAvailableChanged(bool isRemovingAvailable);
    void isEmptyChanged();

private slots:
    void updateRearrangementAvailability();
    void updateMovingUpAvailability(bool isSelectionMovable, const QModelIndex& firstSelectedRowIndex = QModelIndex());
    void updateMovingDownAvailability(bool isSelectionMovable, const QModelIndex& lastSelectedRowIndex = QModelIndex());
    void updateRemovingAvailability();

private:
    enum RoleNames {
        rItemData = Qt::UserRole + 1
    };

    bool removeRows(int row, int count, const QModelIndex& parent) override;

    void onProjectChanged();

    TrackItem* buildTrackItem(const processing::Track& track);
    TrackItem* findTrackItem(const muse::ID& trackId);

    void setLoadingBlocked(bool blocked);

    void listenTracksSelectionChanged();
    void updateSelectedRows();

    void clear();
    void deleteItems();

    void setIsMovingUpAvailable(bool isMovingUpAvailable);
    void setIsMovingDownAvailable(bool isMovingDownAvailable);
    void setIsRemovingAvailable(bool isRemovingAvailable);

    void setItemsSelected(const QModelIndexList& indexes, bool selected);

    TrackItem* modelIndexToItem(const QModelIndex& index) const;

    bool m_isMovingUpAvailable = false;
    bool m_isMovingDownAvailable = false;
    bool m_isRemovingAvailable = false;
    bool m_isLoadingBlocked = false;
    bool m_projectChangedWhileLoadingWasBlocked = false;

    bool m_dragInProgress = false;

    QList<TrackItem*> m_trackList;
    muse::uicomponents::ItemMultiSelectionModel* m_selectionModel = nullptr;
    std::shared_ptr<muse::async::Asyncable> m_tracksNotifyReceiver = nullptr;
};
}

#endif // AU_PROJECTSCENE_TRACKSLISTMODEL_H
