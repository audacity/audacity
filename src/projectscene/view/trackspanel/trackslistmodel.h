#ifndef AU_PROJECTSCENE_TRACKSLISTMODEL_H
#define AU_PROJECTSCENE_TRACKSLISTMODEL_H

#include <QAbstractListModel>

#include "actions/iactionsdispatcher.h"
#include "global/async/asyncable.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "types/projectscenetypes.h"
#include "trackedit/iselectioncontroller.h"

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

    muse::Inject<trackedit::ISelectionController> selectionController;
    muse::Inject<trackedit::ITrackeditInteraction> trackeditInteraction;
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher;

public:
    TracksListModel(QObject* parent = nullptr);
    ~TracksListModel() override;

    Q_INVOKABLE void load();

    Q_INVOKABLE void addTrack(TrackTypes::Type type);
    Q_INVOKABLE void duplicateTrack(int row);

    Q_INVOKABLE QItemSelectionModel* selectionModel() const;
    Q_INVOKABLE void selectRow(int row, bool exclusive = false);
    Q_INVOKABLE void clearSelection();
    Q_INVOKABLE void moveSelectedRowsUp();
    Q_INVOKABLE void moveSelectedRowsDown();
    Q_INVOKABLE void removeSelectedRows();

    Q_INVOKABLE void requestTracksMove(QVariantList trackIndexes, int to);

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

    bool isProjectOpened() const;

    bool removeRows(int row, int count, const QModelIndex& parent) override;

    void onProjectChanged();
    void onSelectedTracks(const trackedit::TrackIdList& tracksIds);
    void onTracksChanged(const std::vector<trackedit::Track>& tracks);
    void onTrackAdded(const trackedit::Track& track);
    void onTrackRemoved(const trackedit::Track& track);
    void onTrackChanged(const trackedit::Track& track);
    void onTrackInserted(const trackedit::Track& track, int pos);
    void onTrackMoved(const trackedit::Track& track, int pos);

    TrackItem* buildTrackItem(const trackedit::Track& track);
    TrackItem* findTrackItem(const trackedit::TrackId& trackId);

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
    bool m_audioDataSelected = false;

    QList<TrackItem*> m_trackList;
    muse::uicomponents::ItemMultiSelectionModel* m_selectionModel = nullptr;
};
}

#endif // AU_PROJECTSCENE_TRACKSLISTMODEL_H
