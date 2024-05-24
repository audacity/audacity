#pragma once

#include <QObject>

#include "modularity/ioc.h"
#include "itracksgeometrycontroller.h"

#include "global/async/asyncable.h"

namespace au::projectscene {
class TrackGeometryModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(QVariant trackId READ trackId WRITE setTrackId NOTIFY trackIdChanged FINAL)

    Q_PROPERTY(int trackHeight READ trackHeight NOTIFY trackHeightChanged FINAL)

    muse::Inject<ITracksGeometryController> geometryController;

public:
    TrackGeometryModel(QObject* parent = nullptr);

    QVariant trackId() const;
    void setTrackId(const QVariant& newTrackId);
    int trackHeight() const;

    Q_INVOKABLE void changeTrackHeight(int deltaY);

signals:
    void trackIdChanged();
    void trackHeightChanged();

private:

    void setup(processing::TrackId trackId);

    processing::TrackId m_trackId = -1;
    muse::ValCh<int> m_trackHeight;
};
}
