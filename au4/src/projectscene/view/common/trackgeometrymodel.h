#pragma once

#include <QObject>

#include "global/async/asyncable.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

namespace au::projectscene {
class TrackViewStateModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(QVariant trackId READ trackId WRITE setTrackId NOTIFY trackIdChanged FINAL)

    Q_PROPERTY(int trackHeight READ trackHeight NOTIFY trackHeightChanged FINAL)

    muse::Inject<context::IGlobalContext> globalContext;

public:
    TrackViewStateModel(QObject* parent = nullptr);

    QVariant trackId() const;
    void setTrackId(const QVariant& newTrackId);
    int trackHeight() const;

    Q_INVOKABLE void changeTrackHeight(int deltaY);

signals:
    void trackIdChanged();
    void trackHeightChanged();

private:

    void setup(processing::TrackId trackId);

    IProjectViewStatePtr viewState() const;

    processing::TrackId m_trackId = -1;
    muse::ValCh<int> m_trackHeight;
};
}
