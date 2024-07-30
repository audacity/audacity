/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QAbstractListModel>

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "processing/iprocessinginteraction.h"
#include "processing/iselectioncontroller.h"
#include "actions/iactionsdispatcher.h"
#include "actions/actionable.h"

#include "global/async/asyncable.h"
#include "processing/processingtypes.h"

#include "../timeline/timelinecontext.h"

#include "cliplistitem.h"

namespace au::projectscene {
class ClipsListModel : public QAbstractListModel, public muse::async::Asyncable, public muse::actions::Actionable
{
    Q_OBJECT

    Q_PROPERTY(TimelineContext * context READ timelineContext WRITE setTimelineContext NOTIFY timelineContextChanged FINAL)
    Q_PROPERTY(QVariant trackId READ trackId WRITE setTrackId NOTIFY trackIdChanged FINAL)

    Q_PROPERTY(int cacheBufferPx READ cacheBufferPx CONSTANT)

    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<processing::IProcessingInteraction> processingInteraction;
    muse::Inject<processing::ISelectionController> selectionController;
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher;

public:
    ClipsListModel(QObject* parent = nullptr);

    TimelineContext* timelineContext() const;
    void setTimelineContext(TimelineContext* newContext);
    QVariant trackId() const;
    void setTrackId(const QVariant& newTrackId);
    int selectedClipIdx() const;
    void setSelectedClipIdx(int newSelectedClipIdx);

    Q_INVOKABLE void init();
    Q_INVOKABLE void reload();
    Q_INVOKABLE bool modeClip(const ClipKey& key, double deltaX);
    Q_INVOKABLE void selectClip(const ClipKey& key);
    Q_INVOKABLE void unselectClip(const ClipKey& key);
    Q_INVOKABLE void resetSelectedClip();
    Q_INVOKABLE bool changeClipTitle(const ClipKey& key, const QString& newTitle);

    int rowCount(const QModelIndex& parent) const override;
    QHash<int, QByteArray> roleNames() const override;
    QVariant data(const QModelIndex& index, int role) const override;

    int cacheBufferPx() const;

signals:
    void trackIdChanged();
    void timelineContextChanged();
    void selectedClipIdxChanged();

    void requestClipTitleEdit(int index);

    void contentXChanged();

private slots:
    void onTimelineZoomChanged();
    void onTimelineFrameTimeChanged();

private:

    enum RoleNames {
        ClipItemRole = Qt::UserRole + 1,
    };

    void update();
    void updateItemsMetrics();
    void positionViewAtClip(const processing::Clip& clip);
    void onSelectedClip(const processing::ClipKey& k);
    void onClipRenameAction(const muse::actions::ActionData& args);
    ClipListItem* itemByKey(const processing::ClipKey& k) const;

    TimelineContext* m_context = nullptr;
    processing::TrackId m_trackId = -1;
    muse::async::NotifyList<au::processing::Clip> m_allClipList;
    QList<ClipListItem*> m_clipList;
    ClipListItem* m_selectedItem = nullptr;
};
}
