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

#include "trackedit/iselectioncontroller.h"
#include "trackedit/itrackeditinteraction.h"
#include "trackedit/trackedittypes.h"
#include "../timeline/timelinecontext.h"

#include "labellistitem.h"

namespace au::projectscene {
class LabelsListModel : public QAbstractListModel, public muse::async::Asyncable, public muse::actions::Actionable
{
    Q_OBJECT

    Q_PROPERTY(TimelineContext * context READ timelineContext WRITE setTimelineContext NOTIFY timelineContextChanged FINAL)
    Q_PROPERTY(QVariant trackId READ trackId WRITE setTrackId NOTIFY trackIdChanged FINAL)

    Q_PROPERTY(int cacheBufferPx READ cacheBufferPx CONSTANT)

    muse::Inject<muse::actions::IActionsDispatcher> dispatcher;
    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<muse::IInteractive> interactive;
    muse::Inject<trackedit::ITrackeditInteraction> trackeditInteraction;
    muse::Inject<trackedit::ISelectionController> selectionController;

public:
    explicit LabelsListModel(QObject* parent = nullptr);
    ~LabelsListModel() override;

    TimelineContext* timelineContext() const;
    void setTimelineContext(TimelineContext* newContext);
    QVariant trackId() const;
    void setTrackId(const QVariant& newTrackId);

    Q_INVOKABLE void init();
    Q_INVOKABLE void reload();

    Q_INVOKABLE void startEditLabel(const LabelKey& key);
    Q_INVOKABLE void endEditLabel(const LabelKey& key);

    Q_INVOKABLE void selectLabel(const LabelKey& key);
    Q_INVOKABLE void resetSelectedLabels();
    Q_INVOKABLE bool changeLabelTitle(const LabelKey& key, const QString& newTitle);

    Q_INVOKABLE QVariant next(const LabelKey& key) const;
    Q_INVOKABLE QVariant prev(const LabelKey& key) const;

    int rowCount(const QModelIndex& parent) const override;
    QHash<int, QByteArray> roleNames() const override;
    QVariant data(const QModelIndex& index, int role) const override;

    static int cacheBufferPx();

signals:
    void trackIdChanged();
    void timelineContextChanged();

private slots:
    void onTimelineZoomChanged();
    void onTimelineFrameTimeChanged();

private:

    enum RoleNames {
        LabelItemRole = Qt::UserRole + 1,
    };

    void setSelectedItems(const QList<LabelListItem*>& items);
    void addSelectedItem(LabelListItem* item);
    void clearSelectedItems();

    void update();
    void updateItemsMetrics();
    void updateItemsMetrics(LabelListItem* item);
    void onSelectedLabel(const trackedit::LabelKey& k);
    void onSelectedLabels(const trackedit::LabelKeyList& keyList);
    LabelListItem* itemByKey(const trackedit::LabelKey& k) const;
    int indexByKey(const trackedit::LabelKey& k) const;
    QVariant neighbor(const LabelKey& key, int offset) const;
    void requestLabelTitleChange();

    Qt::KeyboardModifiers keyboardModifiers() const;

    TimelineContext* m_context = nullptr;
    trackedit::TrackId m_trackId = -1;
    muse::async::NotifyList<au::trackedit::Label> m_allLabelList;
    QList<LabelListItem*> m_labelList;
    QList<LabelListItem*> m_selectedItems;
};
}

