/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QAbstractListModel>

#include "modularity/ioc.h"
#include "actions/actionable.h"
#include "actions/iactionsdispatcher.h"
#include "context/iglobalcontext.h"
#include "global/iinteractive.h"
#include "global/async/asyncable.h"
#include "workspace/iworkspacemanager.h"

#include "trackedit/iselectioncontroller.h"
#include "trackedit/itrackeditinteraction.h"
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
    Q_PROPERTY(ClipStyles::Style clipStyle READ clipStyle NOTIFY clipStyleChanged FINAL)
    Q_PROPERTY(
        bool asymmetricStereoHeightsPossible READ asymmetricStereoHeightsPossible NOTIFY asymmetricStereoHeightsPossibleChanged)

    Q_PROPERTY(int cacheBufferPx READ cacheBufferPx CONSTANT)

    muse::Inject<muse::actions::IActionsDispatcher> dispatcher;
    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<muse::IInteractive> interactive;
    muse::Inject<trackedit::ITrackeditInteraction> trackeditInteraction;
    muse::Inject<trackedit::ISelectionController> selectionController;
    muse::Inject<projectscene::IProjectSceneConfiguration> projectSceneConfiguration;
    muse::Inject<muse::workspace::IWorkspaceManager> workspacesManager;

public:
    explicit ClipsListModel(QObject* parent = nullptr);
    ~ClipsListModel() override;

    TimelineContext* timelineContext() const;
    void setTimelineContext(TimelineContext* newContext);
    QVariant trackId() const;
    void setTrackId(const QVariant& newTrackId);
    bool isStereo() const;
    ClipStyles::Style clipStyle() const;

    Q_INVOKABLE void init();
    Q_INVOKABLE void reload();

    Q_INVOKABLE void startEditClip(const ClipKey& key);
    Q_INVOKABLE void endEditClip(const ClipKey& key);

    Q_INVOKABLE bool moveSelectedClips(const ClipKey& key, bool completed);
    Q_INVOKABLE bool trimLeftClip(const ClipKey& key, bool completed, ClipBoundary::Action action = ClipBoundary::Action::Shrink);
    Q_INVOKABLE bool trimRightClip(const ClipKey& key, bool completed, ClipBoundary::Action action = ClipBoundary::Action::Shrink);
    Q_INVOKABLE bool stretchLeftClip(const ClipKey& key, bool completed, ClipBoundary::Action action = ClipBoundary::Action::Shrink);
    Q_INVOKABLE bool stretchRightClip(const ClipKey& key, bool completed, ClipBoundary::Action action = ClipBoundary::Action::Shrink);

    Q_INVOKABLE void selectClip(const ClipKey& key);
    Q_INVOKABLE void resetSelectedClips();
    Q_INVOKABLE bool changeClipTitle(const ClipKey& key, const QString& newTitle);

    Q_INVOKABLE QVariant next(const ClipKey& key) const;
    Q_INVOKABLE QVariant prev(const ClipKey& key) const;

    Q_INVOKABLE void openClipPitchEdit(const ClipKey& key);
    Q_INVOKABLE void resetClipPitch(const ClipKey& key);

    Q_INVOKABLE void openClipSpeedEdit(const ClipKey& key);
    Q_INVOKABLE void resetClipSpeed(const ClipKey& key);

    Q_INVOKABLE QVariant findGuideline(const ClipKey& key, Direction direction);

    // update clip after moving to other track
    Q_INVOKABLE projectscene::ClipKey updateClipTrack(ClipKey clipKey) const;

    bool asymmetricStereoHeightsPossible() const;

    int rowCount(const QModelIndex& parent) const override;
    QHash<int, QByteArray> roleNames() const override;
    QVariant data(const QModelIndex& index, int role) const override;

    static int cacheBufferPx();

signals:
    void trackIdChanged();
    void timelineContextChanged();
    void selectedClipIdxChanged();
    void isStereoChanged();
    void clipStyleChanged();
    void asymmetricStereoHeightsPossibleChanged();

    void requestClipTitleEdit(int index);

    void contentXChanged();

private slots:
    void onTimelineZoomChanged();
    void onTimelineFrameTimeChanged();

private:

    enum RoleNames {
        ClipItemRole = Qt::UserRole + 1,
    };

    struct MoveOffset {
        muse::secs_t timeOffset = 0.0;
        int trackOffset = 0;
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
    ClipListItem* itemByKey(const trackedit::ClipKey& k) const;
    int indexByKey(const trackedit::ClipKey& k) const;
    QVariant neighbor(const ClipKey& key, int offset) const;
    void requestClipTitleChange();

    MoveOffset calculateMoveOffset(const ClipListItem* item, const ClipKey& key, bool completed) const;
    trackedit::secs_t calculateTimePositionOffset(const ClipListItem* item) const;
    int calculateTrackPositionOffset(const ClipKey& key) const;
    bool isKeyboardTriggered() const;

    void handleAutoScroll(bool ok, bool completed, const std::function<void()>& onAutoScrollFrame);
    void disconnectAutoScroll();

    Qt::KeyboardModifiers keyboardModifiers() const;

    TimelineContext* m_context = nullptr;
    trackedit::TrackId m_trackId = -1;
    muse::async::NotifyList<au::trackedit::Clip> m_allClipList;
    QList<ClipListItem*> m_clipList;
    QList<ClipListItem*> m_selectedItems;
    bool m_isStereo = false;
    ClipStyles::Style m_clipStyle = ClipStyles::Style::COLORFUL;

    QMetaObject::Connection m_autoScrollConnection;
};
}
