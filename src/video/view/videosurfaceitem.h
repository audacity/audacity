/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_VIDEO_VIDEOSURFACEITEM_H
#define AU_VIDEO_VIDEOSURFACEITEM_H

#include <QImage>
#include <QQuickPaintedItem>

#include "context/iglobalcontext.h"
#include "global/async/asyncable.h"
#include "modularity/ioc.h"

#include "../ivideoservice.h"

namespace au::video {
//! Shows the frame at the playhead.
//!
//! Derives from QQuickPaintedItem directly rather than from muse's
//! QuickPaintedView. That base class pins the texture size to the item's
//! logical size to keep one-pixel UI rules crisp, which is right for chrome
//! and wrong here: it would render video at half resolution on any 2x display.
class VideoSurfaceItem : public QQuickPaintedItem, public muse::async::Asyncable,
    public muse::Contextable
{
    Q_OBJECT

    Q_PROPERTY(bool hasFrame READ hasFrame NOTIFY frameChanged FINAL)
    Q_PROPERTY(bool outOfRange READ outOfRange NOTIFY frameChanged FINAL)

public:
    explicit VideoSurfaceItem(QQuickItem* parent = nullptr);

    bool hasFrame() const;
    bool outOfRange() const;

    void paint(QPainter* painter) override;

    //! Injected services resolve through the QQmlContext, which QML sets after
    //! the constructor has run. Touching a ContextInject any earlier resolves
    //! it to null and crashes on first use, so all wiring happens here rather
    //! than in the constructor.
    void componentComplete() override;

signals:
    void frameChanged();

private:
    void onPositionChanged(muse::secs_t position);
    void requestFrame(muse::secs_t position);

    //! Target size in device pixels. Decoding at logical size would render at
    //! half resolution on a 2x display, and tearing the panel to a 1x screen
    //! silently changes the answer, so this is re-read every request.
    QSize targetPixelSize() const;

    muse::ContextInject<context::IGlobalContext> globalContext { this };
    muse::ContextInject<IVideoService> videoService { this };

    QImage m_image;
    muse::secs_t m_lastPosition = -1.0;
    bool m_subscribed = false;
    bool m_outOfRange = false;
};
}

#endif // AU_VIDEO_VIDEOSURFACEITEM_H
