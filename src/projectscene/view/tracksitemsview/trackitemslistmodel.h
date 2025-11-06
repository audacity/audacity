/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <functional>

#include <QAbstractListModel>

#include "actions/actionable.h"
#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "global/iinteractive.h"
#include "context/iglobalcontext.h"
#include "actions/iactionsdispatcher.h"
#include "trackedit/iselectioncontroller.h"
#include "trackedit/itrackeditinteraction.h"
#include "trackedit/trackedittypes.h"
#include "trackedit/iprojecthistory.h"

#include "../timeline/timelinecontext.h"

#include "types/projectscenetypes.h"
#include "viewtrackitem.h"

namespace au::projectscene {
class TrackItemsListModel : public QAbstractListModel, public muse::async::Asyncable, public muse::actions::Actionable
{
    Q_OBJECT

    Q_PROPERTY(TimelineContext * context READ timelineContext WRITE setTimelineContext NOTIFY timelineContextChanged FINAL)
    Q_PROPERTY(QVariant trackId READ trackId WRITE setTrackId NOTIFY trackIdChanged FINAL)
    Q_PROPERTY(int cacheBufferPx READ cacheBufferPx CONSTANT)

protected:
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher;
    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<muse::IInteractive> interactive;
    muse::Inject<trackedit::ITrackeditInteraction> trackeditInteraction;
    muse::Inject<trackedit::ISelectionController> selectionController;
    muse::Inject<trackedit::IProjectHistory> projectHistory;

public:
    explicit TrackItemsListModel(QObject* parent = nullptr);
    ~TrackItemsListModel() override;

    TimelineContext* timelineContext() const;
    void setTimelineContext(TimelineContext* newContext);
    QVariant trackId() const;
    void setTrackId(const QVariant& newTrackId);

    static int cacheBufferPx();

    Q_INVOKABLE void init();
    Q_INVOKABLE void reload();

    Q_INVOKABLE void startEditItem(const TrackItemKey& key);
    Q_INVOKABLE void endEditItem(const TrackItemKey& key);
    Q_INVOKABLE bool cancelItemDragEdit(const TrackItemKey& key);

    Q_INVOKABLE QVariant next(const TrackItemKey& key) const;
    Q_INVOKABLE QVariant prev(const TrackItemKey& key) const;

    int rowCount(const QModelIndex& parent) const override;
    QHash<int, QByteArray> roleNames() const override;
    QVariant data(const QModelIndex& index, int role) const override;

signals:
    void trackIdChanged();
    void timelineContextChanged();
    void itemTitleEditRequested(const TrackItemKey& key);

protected slots:
    virtual void onTimelineZoomChanged();
    virtual void onTimelineFrameTimeChanged();

protected:
    enum RoleNames {
        ItemRole = Qt::UserRole + 1,
    };

    void updateItemsMetrics();
    virtual void updateItemMetrics(ViewTrackItem* item) = 0;

    void setSelectedItems(const QList<ViewTrackItem*>& items);
    void addSelectedItem(ViewTrackItem* item);
    void clearSelectedItems();

    ViewTrackItem* itemByKey(const trackedit::TrackItemKey& key) const;
    int indexByKey(const trackedit::TrackItemKey& key) const;
    virtual void onStartEditItem(const trackedit::TrackItemKey&) {}
    virtual void onEndEditItem(const trackedit::TrackItemKey&) {}

    void requestItemTitleChange();
    virtual trackedit::TrackItemKeyList getSelectedItemKeys() const = 0;

    virtual void onInit() = 0;
    virtual void onReload() = 0;

    void onSelectedItem(const trackedit::TrackItemKey& k);
    void onSelectedItems(const trackedit::TrackItemKeyList& keyList);

    void handleAutoScroll(bool ok, bool completed, const std::function<void()>& onAutoScrollFrame);
    void disconnectAutoScroll();

    QVariant neighbor(const TrackItemKey& key, int offset) const;

    struct MoveOffset {
        muse::secs_t timeOffset = 0.0;
        int trackOffset = 0;
    };
    MoveOffset calculateMoveOffset(const ViewTrackItem* item, const TrackItemKey& key,
                                   const std::vector<trackedit::TrackType>& trackTypesAllowedToMove, bool completed) const;
    trackedit::secs_t calculateTimePositionOffset(const ViewTrackItem* item) const;

    int calculateTrackPositionOffset(const TrackItemKey& key, const std::vector<trackedit::TrackType>& trackTypesAllowedToMove) const;
    bool isAllowedToMoveToTracks(const std::vector<trackedit::TrackType>& allowedTrackTypes, const trackedit::TrackId& movedTrackId) const;

    Qt::KeyboardModifiers keyboardModifiers() const;

    TimelineContext* m_context = nullptr;
    trackedit::TrackId m_trackId = -1;
    QList<ViewTrackItem*> m_items;
    QList<ViewTrackItem*> m_selectedItems;
    QMetaObject::Connection m_autoScrollConnection;
};
}
