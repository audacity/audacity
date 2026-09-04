/*
* Audacity: A Digital Audio Editor
*/
#include "videosurfaceitem.h"

#include <QPainter>
#include <QQuickWindow>
#include <QtMath>

using namespace au::video;

VideoSurfaceItem::VideoSurfaceItem(QQuickItem* parent)
    : QQuickPaintedItem(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
    setFlag(ItemHasContents, true);
}

void VideoSurfaceItem::componentComplete()
{
    QQuickPaintedItem::componentComplete();

    if (m_subscribed) {
        return;
    }
    m_subscribed = true;

    // The position channel is the one signal that matters: it fires on every
    // seek while stopped, and on every tick of the player's own timer while
    // playing, so following it covers both halves of "in sync".
    globalContext()->playbackState()->playbackPositionChanged().onReceive(
        this, [this](muse::secs_t position) { onPositionChanged(position); });

    videoService()->attachedChanged().onNotify(this, [this]() {
        m_lastPosition = -1.0;
        requestFrame(globalContext()->playbackState()->playbackPosition());
    });

    const auto refresh = [this]() {
        m_lastPosition = -1.0;
        requestFrame(globalContext()->playbackState()->playbackPosition());
    };
    connect(this, &QQuickItem::widthChanged, this, refresh);
    connect(this, &QQuickItem::heightChanged, this, refresh);

    refresh();
}

bool VideoSurfaceItem::hasFrame() const
{
    return !m_image.isNull();
}

bool VideoSurfaceItem::outOfRange() const
{
    return m_outOfRange;
}

QSize VideoSurfaceItem::targetPixelSize() const
{
    const qreal dpr = window() != nullptr ? window()->effectiveDevicePixelRatio() : 1.0;
    return QSize(qMax(1, qCeil(width() * dpr)), qMax(1, qCeil(height() * dpr)));
}

void VideoSurfaceItem::onPositionChanged(muse::secs_t position)
{
    // The player publishes unconditionally on seek, so identical values arrive
    // repeatedly while a playhead is held still. Decoding those again would be
    // pure waste.
    if (qFuzzyCompare(position.to_double() + 1.0, m_lastPosition.to_double() + 1.0)) {
        return;
    }
    requestFrame(position);
}

void VideoSurfaceItem::requestFrame(muse::secs_t position)
{
    if (!m_subscribed || width() <= 0 || height() <= 0) {
        return;
    }

    if (!videoService()->isAttached()) {
        if (!m_image.isNull()) {
            m_image = QImage();
            emit frameChanged();
            update();
        }
        return;
    }

    // Past the end of the video there is nothing to decode. Clear rather than
    // leaving the last frame up, which would read as though it were current.
    if (!videoService()->isTimeInRange(position)) {
        m_lastPosition = position;
        if (!m_outOfRange || !m_image.isNull()) {
            m_outOfRange = true;
            m_image = QImage();
            emit frameChanged();
            update();
        }
        return;
    }
    m_outOfRange = false;

    const QSize target = targetPixelSize();
    const VideoFrame frame = videoService()->frameAt(position, target.width(), target.height());

    m_lastPosition = position;

    if (!frame.valid()) {
        // Hold the previous frame rather than blanking. A momentary decode miss
        // should not flash the panel to black.
        return;
    }

    m_image = frame.image;
    emit frameChanged();
    update();
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
