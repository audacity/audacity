#pragma once

#include <QObject>

#include "global/async/asyncable.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

namespace au::projectscene {
//! NOTE This is model need for synchronization of view (scroll, height and so on)
//! Operation in context of track (if trackId set)
//! and context of all tracks (if trackId not set)
class TracksViewStateModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    // context of all tracks
    Q_PROPERTY(int tracksVericalY READ tracksVericalY NOTIFY tracksVericalYChanged FINAL)

    // context of track
    Q_PROPERTY(QVariant trackId READ trackId WRITE setTrackId NOTIFY trackIdChanged FINAL)
    Q_PROPERTY(int trackHeight READ trackHeight NOTIFY trackHeightChanged FINAL)

    muse::Inject<context::IGlobalContext> globalContext;

public:
    TracksViewStateModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();

    // context of all tracks
    int tracksVericalY() const;
    Q_INVOKABLE void changeTracksVericalY(int deltaY);

    // context of track
    QVariant trackId() const;
    void setTrackId(const QVariant& newTrackId);
    int trackHeight() const;

    Q_INVOKABLE void changeTrackHeight(int deltaY);

signals:
    // context of all tracks
    void tracksVericalYChanged();

    // context of track
    void trackIdChanged();
    void trackHeightChanged();

private:

    IProjectViewStatePtr viewState() const;

    // context of all tracks
    muse::ValCh<int> m_tracksVericalY;

    // context of track
    processing::TrackId m_trackId = -1;
    muse::ValCh<int> m_trackHeight;
};
}
