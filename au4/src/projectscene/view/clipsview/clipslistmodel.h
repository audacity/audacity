#pragma once

#include <QAbstractListModel>

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "processing/iprocessinginteraction.h"

#include "global/async/asyncable.h"
#include "processing/processingtypes.h"

#include "timelinecontext.h"

namespace au::projectscene {
class ClipsListModel : public QAbstractListModel, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(TimelineContext * context READ timelineContext WRITE setTimelineContext NOTIFY timelineContextChanged FINAL)
    Q_PROPERTY(QVariant trackId READ trackId WRITE setTrackId NOTIFY trackIdChanged FINAL)

    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<processing::IProcessingInteraction> processingInteraction;

public:
    ClipsListModel(QObject* parent = nullptr);

    Q_INVOKABLE void load();
    Q_INVOKABLE void onSelected(double x1, double x2);
    Q_INVOKABLE void resetSelection();

    int rowCount(const QModelIndex& parent) const override;
    QHash<int, QByteArray> roleNames() const override;
    QVariant data(const QModelIndex& index, int role) const override;
    bool setData(const QModelIndex& index, const QVariant& value, int role = Qt::EditRole) override;

    QVariant trackId() const;
    void setTrackId(const QVariant& newTrackId);

    TimelineContext* timelineContext() const;
    void setTimelineContext(TimelineContext* newContext);

signals:
    void trackIdChanged();
    void timelineContextChanged();

private slots:
    void onTimelineContextValuesChanged();

private:

    enum RoleNames {
        ClipKeyRole = Qt::UserRole + 1,
        ClipTitleRole,
        ClipWidthRole,
        ClipLeftRole
    };

    bool changeClipStartTime(const QModelIndex& index, const QVariant& value);
    void onSelectedTime(double startTime, double endTime);

    processing::TrackId m_trackId = -1;
    muse::async::NotifyList<au::processing::Clip> m_clipList;
    TimelineContext* m_context = nullptr;
};
}
