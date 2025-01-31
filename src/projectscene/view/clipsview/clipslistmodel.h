/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QAbstractListModel>

#include "actions/actionable.h"

#include "modularity/ioc.h"
#include "actions/iactionsdispatcher.h"
#include "context/iglobalcontext.h"
#include "global/iinteractive.h"
#include "trackedit/itrackeditinteraction.h"
#include "trackedit/iselectioncontroller.h"

#include "global/async/asyncable.h"
#include "trackedit/trackedittypes.h"

#include "../timeline/timelinecontext.h"

#include "cliplistitem.h"

namespace au::projectscene {
class ClipsListModel : public QAbstractListModel, public muse::async::Asyncable, public muse::actions::Actionable
{
    Q_OBJECT

    Q_PROPERTY(TimelineContext * context READ timelineContext WRITE setTimelineContext NOTIFY timelineContextChanged FINAL)
    Q_PROPERTY(QVariant trackId READ trackId WRITE setTrackId NOTIFY trackIdChanged FINAL)
    Q_PROPERTY(bool isStereo READ isStereo NOTIFY isStereoChanged FINAL)

    Q_PROPERTY(int cacheBufferPx READ cacheBufferPx CONSTANT)

    muse::Inject<muse::actions::IActionsDispatcher> dispatcher;
    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<muse::IInteractive> interactive;
    muse::Inject<trackedit::ITrackeditInteraction> trackeditInteraction;
    muse::Inject<trackedit::ISelectionController> selectionController;

public:
    ClipsListModel(QObject* parent = nullptr);
    ~ClipsListModel();

    TimelineContext* timelineContext() const;
    void setTimelineContext(TimelineContext* newContext);
    QVariant trackId() const;
    void setTrackId(const QVariant& newTrackId);
    int selectedClipIdx() const;
    void setSelectedClipIdx(int newSelectedClipIdx);
    bool isStereo() const;

    Q_INVOKABLE void init();
    Q_INVOKABLE void reload();

    Q_INVOKABLE void startEditClip(const ClipKey& key);
    Q_INVOKABLE void endEditClip(const ClipKey& key);

    Q_INVOKABLE bool moveSelectedClips(const ClipKey& key, bool completed);
    Q_INVOKABLE bool trimLeftClip(const ClipKey& key, bool completed);
    Q_INVOKABLE bool trimRightClip(const ClipKey& key, bool completed);
    Q_INVOKABLE bool stretchLeftClip(const ClipKey& key, bool completed);
    Q_INVOKABLE bool stretchRightClip(const ClipKey& key, bool completed);

    Q_INVOKABLE void selectClip(const ClipKey& key);
    Q_INVOKABLE void resetSelectedClips();
    Q_INVOKABLE bool changeClipTitle(const ClipKey& key, const QString& newTitle);

    Q_INVOKABLE QVariant next(const ClipKey& key) const;
    Q_INVOKABLE QVariant prev(const ClipKey& key) const;

    Q_INVOKABLE void openClipPitchEdit(const ClipKey& key);
    Q_INVOKABLE void resetClipPitch(const ClipKey& key);

    Q_INVOKABLE void openClipSpeedEdit(const ClipKey& key);
    Q_INVOKABLE void resetClipSpeed(const ClipKey& key);

    // update clip after moving to other track
    Q_INVOKABLE projectscene::ClipKey updateClipTrack(ClipKey clipKey) const;

    int rowCount(const QModelIndex& parent) const override;
    QHash<int, QByteArray> roleNames() const override;
    QVariant data(const QModelIndex& index, int role) const override;

    int cacheBufferPx() const;

signals:
    void trackIdChanged();
    void timelineContextChanged();
    void selectedClipIdxChanged();
    void isStereoChanged();

    void requestClipTitleEdit(int index);

    void contentXChanged();

private slots:
    void onTimelineZoomChanged();
    void onTimelineFrameTimeChanged();

private:

    enum RoleNames {
        ClipItemRole = Qt::UserRole + 1,
    };

    void setSelectedItems(const QList<ClipListItem*>& items);
    void addSelectedItem(ClipListItem* item);
    void clearSelectedItems();

    void update();
    void updateItemsMetrics();
    void updateItemsMetrics(ClipListItem* item);
    void positionViewAtClip(const trackedit::Clip& clip);
    void onSelectedClip(const trackedit::ClipKey& k);
    void onSelectedClips(const trackedit::ClipKeyList& keyList);
    void onClipRenameAction(const muse::actions::ActionData& args);
    ClipListItem* itemByKey(const trackedit::ClipKey& k) const;
    int indexByKey(const trackedit::ClipKey& k) const;
    QVariant neighbor(const ClipKey& key, int offset) const;

    trackedit::secs_t calculateTimePositionOffset(const ClipListItem* item) const;
    int calculateTrackPositionOffset(const ClipKey& key, bool completed) const;

    Qt::KeyboardModifiers keyboardModifiers() const;

    TimelineContext* m_context = nullptr;
    trackedit::TrackId m_trackId = -1;
    muse::async::NotifyList<au::trackedit::Clip> m_allClipList;
    QList<ClipListItem*> m_clipList;
    QList<ClipListItem*> m_selectedItems;
    bool m_isStereo = false;

    QMetaObject::Connection m_autoScrollConnection;
};
}
