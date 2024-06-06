#pragma once

#include <QAbstractListModel>

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "processing/iprocessinginteraction.h"

#include "global/async/asyncable.h"
#include "processing/processingtypes.h"

#include "../timeline/timelinecontext.h"

namespace au::projectscene {
class ClipsListModel : public QAbstractListModel, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(TimelineContext * context READ timelineContext WRITE setTimelineContext NOTIFY timelineContextChanged FINAL)
    Q_PROPERTY(QVariant trackId READ trackId WRITE setTrackId NOTIFY trackIdChanged FINAL)
    Q_PROPERTY(int selectedClipIdx READ selectedClipIdx NOTIFY selectedClipIdxChanged FINAL)

    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<processing::IProcessingInteraction> processingInteraction;

public:
    ClipsListModel(QObject* parent = nullptr);

    TimelineContext* timelineContext() const;
    void setTimelineContext(TimelineContext* newContext);
    QVariant trackId() const;
    void setTrackId(const QVariant& newTrackId);
    int selectedClipIdx() const;
    void setSelectedClipIdx(int newSelectedClipIdx);

    Q_INVOKABLE void load();
    Q_INVOKABLE void selectClip(int index);
    Q_INVOKABLE void resetSelectedClip();

    Q_INVOKABLE void onSelected(double x1, double x2);
    Q_INVOKABLE void resetSelection();

    int rowCount(const QModelIndex& parent) const override;
    QHash<int, QByteArray> roleNames() const override;
    QVariant data(const QModelIndex& index, int role) const override;
    bool setData(const QModelIndex& index, const QVariant& value, int role = Qt::EditRole) override;

signals:
    void trackIdChanged();
    void timelineContextChanged();
    void selectedClipIdxChanged();

private slots:
    void onTimelineContextValuesChanged();

private:

    enum RoleNames {
        ClipKeyRole = Qt::UserRole + 1,
        ClipTitleRole,
        ClipColorRole,
        ClipWidthRole,
        ClipLeftRole
    };

    bool changeClipStartTime(const QModelIndex& index, const QVariant& value);
    bool changeClipTitle(const QModelIndex& index, const QVariant& value);

    void onSelectedTime(double startTime, double endTime);
    void onSelectedClip(const processing::ClipKey& k);

    TimelineContext* m_context = nullptr;
    processing::TrackId m_trackId = -1;
    muse::async::NotifyList<au::processing::Clip> m_clipList;
    int m_selectedClipIdx = -1;
};
}
