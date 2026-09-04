/*
* Audacity: A Digital Audio Editor
*/
#include "videosurfaceitem.h"

#include <chrono>
#include <cmath>

#include <QPainter>
#include <QQuickWindow>
#include <QtMath>

using namespace au::video;

namespace {
//! Matches the player's own position timer. Faster would repaint frames that
//! have not changed; slower would show stale ones at high frame rates.
constexpr int TICK_INTERVAL_MS = 16;
}

VideoSurfaceItem::VideoSurfaceItem(QQuickItem* parent)
    : QQuickPaintedItem(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
    setFlag(ItemHasContents, true);

    m_tick.setInterval(TICK_INTERVAL_MS);
    m_tick.setTimerType(Qt::PreciseTimer);
    connect(&m_tick, &QTimer::timeout, this, [this]() { onTick(); });
}

VideoSurfaceItem::~VideoSurfaceItem()
{
    m_tick.stop();
}

void VideoSurfaceItem::componentComplete()
{
    QQuickPaintedItem::componentComplete();

    if (m_subscribed) {
        return;
    }
    m_subscribed = true;

    applyClockConfig();

    const auto state = globalContext()->playbackState();

    state->playbackPositionChanged().onReceive(
        this, [this](muse::secs_t position) { onPositionReport(position); });

    state->playbackStatusChanged().onReceive(
        this, [this](playback::PlaybackStatus status) { onStatusChanged(status); });

    globalContext()->isRecordingChanged().onNotify(
        this, [this]() { onRecordingChanged(); });

    // One notification covers both the geometry and the enabled flag, so both
    // are re-read every time rather than tracked separately.
    playbackController()->loopRegionChanged().onNotify(
        this, [this]() { applyLoopRegion(); });

    videoService()->attachedChanged().onNotify(this, [this]() {
        applyClockConfig();
        m_haveShown = false;
        refreshNow();
    });

    // A frame the decoder just produced may be the one that should be showing.
    videoService()->frameReady().onNotify(this, [this]() {
        if (!m_tick.isActive()) {
            refreshNow();
        }
    });

    const auto refresh = [this]() { m_haveShown = false; refreshNow(); };
    connect(this, &QQuickItem::widthChanged, this, refresh);
    connect(this, &QQuickItem::heightChanged, this, refresh);

    // Seed it now: the notification only fires on a change, so waiting for
    // one would leave the clock loop-blind until the user touched the region.
    applyLoopRegion();

    m_clock.stop(state->playbackPosition());
    onStatusChanged(state->playbackStatus());
    refreshNow();
}

void VideoSurfaceItem::applyClockConfig()
{
    VideoSyncClock::Config config;

    const VideoStreamInfo& info = videoService()->streamInfo();
    if (info.frameRate > 0.0) {
        config.frameDuration = 1.0 / info.frameRate;
    }

    // The time queue consumes a fixed number of samples per record, so how
    // coarse the position reports are depends on the rate the stream runs at.
    // It reads zero until something has played, and grainForSampleRate falls
    // back for that; this is re-applied when the transport starts, by which
    // point the rate has been negotiated.
    config.grain = VideoSyncClock::grainForSampleRate(
        audioEngine() != nullptr ? audioEngine()->getPlaybackSampleRate() : 0.0);

    m_clock.setConfig(config);
}

void VideoSurfaceItem::applyLoopRegion()
{
    VideoSyncClock::LoopRegion loop;

    // isLoopRegionActive() is asked first, and not only for readability: it
    // tolerates there being no current project, where loopRegion() does not.
    loop.active = playbackController()->isLoopRegionActive();
    if (loop.active) {
        const playback::PlaybackRegion region = playbackController()->loopRegion();
        loop.start = region.start.to_double();
        loop.end = region.end.to_double();
    }

    m_clock.setLoopRegion(loop);
}

bool VideoSurfaceItem::shouldAdvance() const
{
    // Recording advances the playhead without the transport being "playing",
    // and following it is the whole point of sync-to-picture work: voiceover,
    // dialogue replacement, foley. Keep rolling.
    if (globalContext()->isRecording()) {
        return true;
    }
    return m_haveStatus && m_lastStatus == playback::PlaybackStatus::Running;
}

void VideoSurfaceItem::onStatusChanged(playback::PlaybackStatus status)
{
    // The channel sends on every set, including sets to the value it already
    // had, so a transition is only a transition if the value actually moved.
    if (m_haveStatus && status == m_lastStatus) {
        return;
    }
    m_lastStatus = status;
    m_haveStatus = true;

    const auto state = globalContext()->playbackState();

    if (shouldAdvance()) {
        // The stream rate is only known once a stream is open, so pick it up
        // here rather than keeping the fallback for the whole session.
        applyClockConfig();

        m_clock.start(state->playbackPosition(), std::chrono::steady_clock::now());
        m_tick.start();
    } else {
        // There is no final position report after a stop, so read the value
        // rather than waiting for one that will not arrive.
        m_clock.stop(state->playbackPosition());
        m_tick.stop();
        refreshNow();
    }
}

void VideoSurfaceItem::onRecordingChanged()
{
    const auto state = globalContext()->playbackState();

    if (shouldAdvance()) {
        if (!m_tick.isActive()) {
            m_clock.start(state->playbackPosition(), std::chrono::steady_clock::now());
            m_tick.start();
        }
    } else {
        m_clock.stop(state->playbackPosition());
        m_tick.stop();
        refreshNow();
    }
}

void VideoSurfaceItem::onPositionReport(muse::secs_t position)
{
    // Runs inside the player's own timer tick, so it stays cheap: feed the
    // clock, and ask for a decode. No decoding, no allocation, no file access.
    const auto response = m_clock.onPosition(position, std::chrono::steady_clock::now());

    if (!m_clock.isAdvancing()) {
        // Stopped or paused: the report is a seek, and there is no tick
        // running to pick it up.
        if (response == VideoSyncClock::Response::Reanchored) {
            refreshNow();
        }
        return;
    }

    if (response == VideoSyncClock::Response::Reanchored) {
        showFrameFor(position, true);
    }
}

void VideoSurfaceItem::onTick()
{
    const auto now = std::chrono::steady_clock::now();

    if (m_clock.isStalled(now)) {
        // The player's own position freezes when the audio callback stops
        // delivering. Freeze with it rather than running on into a gap.
        return;
    }

    showFrameFor(m_clock.advanceTo(now), true);
}

void VideoSurfaceItem::refreshNow()
{
    const muse::secs_t position = m_clock.isAdvancing()
                                  ? m_clock.position(std::chrono::steady_clock::now())
                                  : globalContext()->playbackState()->playbackPosition();
    showFrameFor(position, true);
}

void VideoSurfaceItem::setOutOfRange(bool outOfRange)
{
    if (m_outOfRange == outOfRange) {
        return;
    }
    m_outOfRange = outOfRange;

    if (m_outOfRange && !m_image.isNull()) {
        m_image = QImage();
        m_haveShown = false;
        update();
    }
    emit frameChanged();
}

void VideoSurfaceItem::showFrameFor(muse::secs_t time, bool requestDecode)
{
    if (width() <= 0 || height() <= 0) {
        return;
    }

    if (!videoService()->isAttached()) {
        if (!m_image.isNull()) {
            m_image = QImage();
            m_haveShown = false;
            emit frameChanged();
            update();
        }
        return;
    }

    // Past the end of the video there is nothing to decode. Clear rather than
    // leaving the last frame up, which would read as though it were current.
    if (!videoService()->isTimeInRange(time)) {
        setOutOfRange(true);
        return;
    }
    setOutOfRange(false);

    bool covers = false;
    const VideoFrame frame = videoService()->cachedFrameAt(time, &covers);

    if (frame.valid() && (!m_haveShown || frame.pts != m_shownPts)) {
        m_image = frame.image;
        m_shownPts = frame.pts;
        m_shownTime = frame.time;
        m_haveShown = true;
        emit frameChanged();
        update();
    }

    if (frame.valid()) {
        const int drift = static_cast<int>(
            std::lround((frame.time.to_double() - time.to_double()) * 1000.0));
        if (drift != m_driftMs) {
            m_driftMs = drift;
            emit driftChanged();
        }
    }

    // Ask for this time whether or not the cache had it. The request slot only
    // keeps the newest, so asking again while already correct costs one
    // superseded entry rather than a decode.
    if (requestDecode && !covers) {
        const QSize target = targetPixelSize();
        videoService()->requestFrame(time, target.width(), target.height());
    }
}

QSize VideoSurfaceItem::targetPixelSize() const
{
    const qreal dpr = window() != nullptr ? window()->effectiveDevicePixelRatio() : 1.0;
    return QSize(qMax(1, qCeil(width() * dpr)), qMax(1, qCeil(height() * dpr)));
}

bool VideoSurfaceItem::hasFrame() const
{
    return !m_image.isNull();
}

qreal VideoSurfaceItem::frameAspect() const
{
    if (m_image.isNull() || m_image.height() <= 0) {
        return 0.0;
    }

    return qreal(m_image.width()) / qreal(m_image.height());
}

bool VideoSurfaceItem::outOfRange() const
{
    return m_outOfRange;
}

int VideoSurfaceItem::driftMs() const
{
    return m_driftMs;
}

int VideoSurfaceItem::frameNumber() const
{
    const double rate = videoService()->streamInfo().frameRate;
    if (rate <= 0.0) {
        return -1;
    }
    return static_cast<int>(std::llround(m_shownTime.to_double() * rate));
}

QString VideoSurfaceItem::frameTimecode() const
{
    if (!m_haveShown) {
        return QString();
    }

    const double t = std::max(0.0, m_shownTime.to_double());
    const int totalMs = static_cast<int>(std::llround(t * 1000.0));

    return QString("%1:%2:%3.%4")
           .arg(totalMs / 3600000, 2, 10, QLatin1Char('0'))
           .arg((totalMs / 60000) % 60, 2, 10, QLatin1Char('0'))
           .arg((totalMs / 1000) % 60, 2, 10, QLatin1Char('0'))
           .arg(totalMs % 1000, 3, 10, QLatin1Char('0'));
}

void VideoSurfaceItem::paint(QPainter* painter)
{
    painter->fillRect(0, 0, static_cast<int>(width()), static_cast<int>(height()), Qt::black);

    if (m_image.isNull()) {
        return;
    }

    // Letterbox: keep the source aspect ratio rather than stretching it.
    const QSizeF target = QSizeF(m_image.size()).scaled(QSizeF(width(), height()), Qt::KeepAspectRatio);
    const QRectF dest((width() - target.width()) / 2.0,
                      (height() - target.height()) / 2.0,
                      target.width(), target.height());

    painter->setRenderHint(QPainter::SmoothPixmapTransform, true);
    painter->drawImage(dest, m_image);
}
