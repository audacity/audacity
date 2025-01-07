/*
* Audacity: A Digital Audio Editor
*/
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
    Q_PROPERTY(bool tracksVerticalScrollLocked READ tracksVerticalScrollLocked NOTIFY tracksVerticalScrollLockedChanged FINAL)
    Q_PROPERTY(int tracksVerticalScrollPadding READ tracksVerticalScrollPadding FINAL)

    // context of track
    Q_PROPERTY(QVariant trackId READ trackId WRITE setTrackId NOTIFY trackIdChanged FINAL)
    Q_PROPERTY(int trackHeight READ trackHeight NOTIFY trackHeightChanged FINAL)
    Q_PROPERTY(bool isTrackCollapsed READ isTrackCollapsed NOTIFY isTrackCollapsedChanged FINAL)

    muse::Inject<context::IGlobalContext> globalContext;

public:
    TracksViewStateModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();

    // context of all tracks
    int tracksVericalY() const;
    Q_INVOKABLE void changeTracksVericalY(int deltaY);
    Q_INVOKABLE void setMouseY(double y);

    Q_INVOKABLE void requestVerticalScrollLock();
    Q_INVOKABLE void requestVerticalScrollUnlock();

    // context of track
    QVariant trackId() const;
    void setTrackId(const QVariant& newTrackId);
    int trackHeight() const;
    bool isTrackCollapsed() const;
    bool tracksVerticalScrollLocked() const;
    int tracksVerticalScrollPadding() const;

    Q_INVOKABLE void changeTrackHeight(int deltaY);

signals:
    // context of all tracks
    void tracksVericalYChanged();
    void tracksVerticalScrollLockedChanged();

    // context of track
    void trackIdChanged();
    void trackHeightChanged();
    void isTrackCollapsedChanged();

private:
    static constexpr int m_tracksVerticalScrollPadding = 228;

    IProjectViewStatePtr viewState() const;

    // context of all tracks
    muse::ValCh<int> m_tracksVericalY;
    muse::ValCh<bool> m_tracksVerticalScrollLocked;

    // context of track
    trackedit::TrackId m_trackId = -1;
    muse::ValCh<int> m_trackHeight;
    muse::ValCh<bool> m_isTrackCollapsed;
};
}
