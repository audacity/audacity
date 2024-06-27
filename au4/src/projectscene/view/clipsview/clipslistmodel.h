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

namespace au::projectscene {
class ClipsListModel : public QAbstractListModel, public muse::async::Asyncable, public muse::actions::Actionable
{
    Q_OBJECT

    Q_PROPERTY(TimelineContext * context READ timelineContext WRITE setTimelineContext NOTIFY timelineContextChanged FINAL)
    Q_PROPERTY(QVariant trackId READ trackId WRITE setTrackId NOTIFY trackIdChanged FINAL)
    Q_PROPERTY(int selectedClipIdx READ selectedClipIdx NOTIFY selectedClipIdxChanged FINAL)

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
    Q_INVOKABLE void selectClip(int index);
    Q_INVOKABLE void resetSelectedClip();

    int rowCount(const QModelIndex& parent) const override;
    QHash<int, QByteArray> roleNames() const override;
    QVariant data(const QModelIndex& index, int role) const override;
    bool setData(const QModelIndex& index, const QVariant& value, int role = Qt::EditRole) override;

signals:
    void trackIdChanged();
    void timelineContextChanged();
    void selectedClipIdxChanged();

    void requestClipTitleEdit(int index);

private slots:
    void onTimelineContextValuesChanged();

private:

    enum RoleNames {
        ClipKeyRole = Qt::UserRole + 1,
        ClipTitleRole,
        ClipColorRole,
        ClipWidthRole,
        ClipLeftRole,
        ClipMoveMaximumXRole,
        ClipMoveMinimumXRole
    };

    void update();

    bool changeClipStartTime(const QModelIndex& index, const QVariant& value);
    bool changeClipTitle(const QModelIndex& index, const QVariant& value);

    void onSelectedClip(const processing::ClipKey& k);

    void onClipRenameAction(const muse::actions::ActionData& args);

    TimelineContext* m_context = nullptr;
    processing::TrackId m_trackId = -1;
    muse::async::NotifyList<au::processing::Clip> m_allClipList;
    std::vector<au::processing::Clip> m_clipList;
    int m_selectedClipIdx = -1;
};
}
