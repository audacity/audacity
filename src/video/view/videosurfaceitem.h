/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_VIDEO_VIDEOSURFACEITEM_H
#define AU_VIDEO_VIDEOSURFACEITEM_H

#include <QImage>
#include <QQuickPaintedItem>
#include <QTimer>

#include "context/iglobalcontext.h"
#include "global/async/asyncable.h"
#include "modularity/ioc.h"
#include "audio/iaudioengine.h"
#include "playback/iplaybackcontroller.h"
#include "playback/playbacktypes.h"

#include "../internal/videosyncclock.h"
#include "../ivideoservice.h"

namespace au::video {
//! Shows the frame at the playhead.
//!
//! Derives from QQuickPaintedItem directly rather than from muse's
//! QuickPaintedView. That base class pins the texture size to the item's
//! logical size to keep one-pixel UI rules crisp, which is right for chrome
//! and wrong here: it would render video at half resolution on any 2x display.
//!
//! Repaints are driven by this item's own timer rather than by the player's
//! position reports, so the picture updates smoothly between them. The reports
//! re-anchor the clock; the clock interpolates; the decoder runs on its own
//! thread and leaves frames in the cache.
class VideoSurfaceItem : public QQuickPaintedItem, public muse::async::Asyncable,
    public muse::Contextable
{
    Q_OBJECT

    Q_PROPERTY(bool hasFrame READ hasFrame NOTIFY frameChanged FINAL)
    Q_PROPERTY(bool outOfRange READ outOfRange NOTIFY frameChanged FINAL)

    //! Width divided by height of the picture currently held, honouring the
    //! stream's pixel aspect ratio. Zero when there is no frame. Lets a
    //! container size itself to the video rather than guessing.
    Q_PROPERTY(qreal frameAspect READ frameAspect NOTIFY frameChanged FINAL)

    //! How far the shown frame is from where the playhead says it should be,
    //! in milliseconds. Published so a sync complaint can be a number rather
    //! than an argument.
    Q_PROPERTY(int driftMs READ driftMs NOTIFY driftChanged FINAL)

    //! Index of the frame actually on screen, derived from its own timestamp
    //! and the stream's frame rate. Taken from the frame rather than from the
    //! interpolating clock, so it is what the eye is seeing.
    Q_PROPERTY(int frameNumber READ frameNumber NOTIFY frameChanged FINAL)

    //! Timestamp of the frame on screen, as hh:mm:ss.mmm.
    Q_PROPERTY(QString frameTimecode READ frameTimecode NOTIFY frameChanged FINAL)

public:
    explicit VideoSurfaceItem(QQuickItem* parent = nullptr);
    ~VideoSurfaceItem() override;

    bool hasFrame() const;
    bool outOfRange() const;
    qreal frameAspect() const;
    int driftMs() const;
    int frameNumber() const;
    QString frameTimecode() const;

    void paint(QPainter* painter) override;

    //! Injected services resolve through the QQmlContext, which QML sets after
    //! the constructor has run. Touching a ContextInject any earlier resolves
    //! it to null and crashes on first use, so all wiring happens here rather
    //! than in the constructor.
    void componentComplete() override;

signals:
    void frameChanged();
    void driftChanged();

private:
    void onPositionReport(muse::secs_t position);
    void onStatusChanged(playback::PlaybackStatus status);
    void onRecordingChanged();
    void onTick();
    void applyLoopRegion();

    void refreshNow();
    void showFrameFor(muse::secs_t time, bool requestDecode);
    void setOutOfRange(bool outOfRange);

    //! Target size in device pixels. Decoding at logical size would render at
    //! half resolution on a 2x display, and tearing the panel to a 1x screen
    //! silently changes the answer, so this is re-read every request.
    QSize targetPixelSize() const;

    void applyClockConfig();
    bool shouldAdvance() const;

    muse::ContextInject<context::IGlobalContext> globalContext { this };
    muse::ContextInject<IVideoService> videoService { this };

    //! Only for the loop region. Transport commands go through the action
    //! dispatcher, never straight to the controller.
    muse::ContextInject<playback::IPlaybackController> playbackController { this };

    //! Only for the stream's sample rate, which sets how coarse the player's
    //! position reports are. Never for the callback queue: that has a single
    //! documented consumer.
    muse::GlobalInject<audio::IAudioEngine> audioEngine;

    VideoSyncClock m_clock;
    QTimer m_tick;

    QImage m_image;
    int64_t m_shownPts = 0;
    bool m_haveShown = false;
    bool m_outOfRange = false;
    int m_driftMs = 0;
    muse::secs_t m_shownTime = 0.0;

    playback::PlaybackStatus m_lastStatus = playback::PlaybackStatus::Stopped;
    bool m_haveStatus = false;
    bool m_subscribed = false;
};
}

#endif // AU_VIDEO_VIDEOSURFACEITEM_H
