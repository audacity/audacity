/*
* Audacity: A Digital Audio Editor
*/
#include "volumepressuremeteritem.h"

#include <QLinearGradient>
#include <QPainter>
#include <QPainterPath>

#include "playback/playbacktypes.h"

using au::playback::PlaybackMeterStyle;

namespace au::projectscene {
namespace {
void buildRoundedRectPath(QPainterPath& path, qreal x, qreal y, qreal w, qreal h, qreal radius, bool roundTop, bool roundBottom)
{
    if (roundTop) {
        path.moveTo(x + radius, y);
        path.lineTo(x + w - radius, y);
        path.arcTo(QRectF(x + w - 2 * radius, y, 2 * radius, 2 * radius), 90, -90);
    } else {
        path.moveTo(x, y);
        path.lineTo(x + w, y);
    }

    if (roundBottom) {
        path.lineTo(x + w, y + h - radius);
        path.arcTo(QRectF(x + w - 2 * radius, y + h - 2 * radius, 2 * radius, 2 * radius), 0, -90);
        path.lineTo(x + radius, y + h);
        path.arcTo(QRectF(x, y + h - 2 * radius, 2 * radius, 2 * radius), 270, -90);
    } else {
        path.lineTo(x + w, y + h);
        path.lineTo(x, y + h);
    }

    if (roundTop) {
        path.lineTo(x, y + radius);
        path.arcTo(QRectF(x, y, 2 * radius, 2 * radius), 180, -90);
    } else {
        path.lineTo(x, y);
    }

    path.closeSubpath();
}

void fillRoundedRect(QPainter& p, const QBrush& brush, qreal x, qreal y, qreal w, qreal h, qreal radius, bool roundTop, bool roundBottom)
{
    QPainterPath path;
    buildRoundedRectPath(path, x, y, w, h, radius, roundTop, roundBottom);
    p.fillPath(path, brush);
}
}

VolumePressureMeterItem::VolumePressureMeterItem(QQuickItem* parent)
    : VolumePressureMeterItemBase(parent)
{
}

void VolumePressureMeterItem::paint(QPainter* painter)
{
    if (!painter) {
        return;
    }
    painter->setRenderHint(QPainter::Antialiasing, true);
    painter->setRenderHint(QPainter::SmoothPixmapTransform, true);

    drawBackground(*painter);

    if (m_showClippedInfo) {
        drawClippedIndicator(*painter);
    }

    drawMeterBar(*painter);
}

void VolumePressureMeterItem::setIndicatorWidth(qreal v)
{
    if (m_indicatorWidth == v) {
        return;
    }
    m_indicatorWidth = v;
    setWidth(v);
    emit indicatorWidthChanged();
    update();
}

void VolumePressureMeterItem::setShowClippedInfo(bool v)
{
    if (m_showClippedInfo == v) {
        return;
    }
    m_showClippedInfo = v;
    emit showClippedInfoChanged();
    update();
}

void VolumePressureMeterItem::setOverloadHeight(int v)
{
    if (m_overloadHeight == v) {
        return;
    }
    m_overloadHeight = v;
    emit overloadHeightChanged();
    update();
}

bool VolumePressureMeterItem::isClipping() const
{
    return updatedVolumePressure() >= maxDisplayedVolumePressure();
}

qreal VolumePressureMeterItem::indicatorHeight() const
{
    return height() - m_overloadHeight;
}

qreal VolumePressureMeterItem::sampleValueToHeight(qreal sampleValue) const
{
    if (!meterModel()) {
        return 0.0;
    }
    return indicatorHeight() * meterModel()->sampleToPosition(sampleValue);
}

void VolumePressureMeterItem::drawBackground(QPainter& p) const
{
    QPainterPath path;
    buildRoundedRectPath(path, 0, 0, m_indicatorWidth, height(), 2, true, true);
    p.fillPath(path, meterBackgroundColor());
}

void VolumePressureMeterItem::drawClippedIndicator(QPainter& p) const
{
    const QColor color = clipped() ? clippedColor() : noClippedColor();
    QPainterPath path;
    buildRoundedRectPath(path, 0, 0, m_indicatorWidth, m_overloadHeight, 2, true, false);
    p.fillPath(path, color);
}

void VolumePressureMeterItem::drawMeterBar(QPainter& p)
{
    if (!meterModel()) {
        return;
    }
    switch (meterModel()->meterStyle()) {
    case PlaybackMeterStyle::MeterStyle::Default:
        drawBarStyleDefault(p);
        break;
    case PlaybackMeterStyle::MeterStyle::RMS:
        drawBarStyleRMS(p);
        break;
    case PlaybackMeterStyle::MeterStyle::Gradient:
        drawBarStyleGradient(p);
        break;
    }
}

void VolumePressureMeterItem::drawBarStyleDefault(QPainter& p)
{
    if (isClipping()) {
        p.fillRect(QRectF(0, 0, m_indicatorWidth, indicatorHeight() + m_overloadHeight), clippedColor());
        return;
    }
    const qreal h = sampleValueToHeight(updatedVolumePressure());
    if (h > 0) {
        fillRoundedRect(p, defaultColor(), 0, height() - h, m_indicatorWidth, h, 2, false, true);
    }
    drawPeakMarkers(p);
}

void VolumePressureMeterItem::drawBarStyleRMS(QPainter& p)
{
    if (isClipping()) {
        p.fillRect(QRectF(0, 0, m_indicatorWidth, indicatorHeight() + m_overloadHeight), clippedColor());
        return;
    }
    const qreal yRMS = sampleValueToHeight(currentRMS());
    const qreal yPeak = sampleValueToHeight(currentVolumePressure());

    if (yRMS > 0) {
        fillRoundedRect(p, rmsColor(), 0, height() - yPeak, m_indicatorWidth, yPeak, 2, false, true);
        p.fillRect(QRectF(0, height() - yPeak, m_indicatorWidth, yPeak - yRMS), rmsOverlayColor());
    }

    drawPeakMarkers(p);
}

void VolumePressureMeterItem::drawBarStyleGradient(QPainter& p)
{
    const qreal h = sampleValueToHeight(updatedVolumePressure());
    if (h > 0) {
        QLinearGradient grad(0, 0, 0, indicatorHeight());
        grad.setColorAt(0.0, gradientColorRed());
        grad.setColorAt(0.2, gradientColorYellow());
        grad.setColorAt(1.0, gradientColorGreen());
        fillRoundedRect(p, grad, 0, height() - h, m_indicatorWidth, h, 2, false, true);
    }
    drawPeakMarkers(p);
}

void VolumePressureMeterItem::drawPeakMarkers(QPainter& p)
{
    const qreal recentH = sampleValueToHeight(recentPeak());
    if (recentH > 0) {
        p.fillRect(QRectF(0, height() - recentH, m_indicatorWidth, 1), recentPeakMarkerColor());
    }
    const qreal maxH = sampleValueToHeight(maxPeak());
    if (maxH > 0) {
        p.fillRect(QRectF(0, height() - maxH, m_indicatorWidth, 1), maxPeakMarkerColor());
    }
}
}
