#pragma once

#include <QAbstractListModel>

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "processing/processingtypes.h"

#include "TimelineContext.h"

namespace au::projectscene {
class ClipsListModel : public QAbstractListModel
{
    Q_OBJECT

    Q_PROPERTY(TimelineContext * context READ timelineContext WRITE setTimelineContext FINAL)
    Q_PROPERTY(QVariant trackId READ trackId WRITE setTrackId NOTIFY trackIdChanged FINAL)

    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    ClipsListModel(QObject* parent = nullptr);

    Q_INVOKABLE void load();

    int rowCount(const QModelIndex& parent) const override;
    QVariant data(const QModelIndex& index, int role) const override;
    QHash<int, QByteArray> roleNames() const override;

    QVariant trackId() const;
    void setTrackId(const QVariant& newTrackId);

    TimelineContext* timelineContext() const;
    void setTimelineContext(TimelineContext* newContext);

signals:
    void trackIdChanged();

private:

    enum RoleNames {
        ClipIndexRole = Qt::UserRole + 1,
        ClipTitleRole,
        ClipWidthRole,
        ClipLeftRole
    };

    processing::TrackId m_trackId = -1;
    muse::async::NotifyList<au::processing::Clip> m_clipList;
    TimelineContext* m_context = nullptr;
};
}
