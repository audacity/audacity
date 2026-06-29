/*
* Audacity: A Digital Audio Editor
*/
#include "videopreviewitem.h"

#include <QPainter>

using namespace au::videopreview;

VideoPreviewItem::VideoPreviewItem(QQuickItem* parent)
    : QQuickPaintedItem(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
    setAntialiasing(false);
}

void VideoPreviewItem::init()
{
    if (m_inited || !service()) {
        return;
    }

    service()->frameChanged().onNotify(this, [this]() {
        update();
    });

    service()->stateChanged().onNotify(this, [this]() {
        update();
    });

    m_inited = true;
}

void VideoPreviewItem::paint(QPainter* painter)
{
    if (!service()) {
        return;
    }

    painter->fillRect(boundingRect(), QColor(10, 10, 10));

    const QImage frame = service()->currentFrame();
    if (frame.isNull()) {
        return;
    }

    const QSizeF targetSize = frame.size().scaled(boundingRect().size().toSize(), Qt::KeepAspectRatio);
    const QRectF targetRect(
        (boundingRect().width() - targetSize.width()) / 2.0,
        (boundingRect().height() - targetSize.height()) / 2.0,
        targetSize.width(),
        targetSize.height());

    painter->setRenderHint(QPainter::SmoothPixmapTransform, true);
    painter->drawImage(targetRect, frame);
}
