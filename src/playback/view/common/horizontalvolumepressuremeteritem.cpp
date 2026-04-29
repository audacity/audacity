/*
* Audacity: A Digital Audio Editor
*/
#include "horizontalvolumepressuremeteritem.h"

#include <QLinearGradient>
#include <QPainter>

#include <cmath>

#include "playbacktypes.h"

namespace au::playback {
HorizontalVolumePressureMeterItem::HorizontalVolumePressureMeterItem(QQuickItem* parent)
    : VolumePressureMeterItemBase(parent)
{
}

void HorizontalVolumePressureMeterItem::paint(QPainter* painter)
{
    if (!painter) {
        return;
    }
    painter->setRenderHint(QPainter::Antialiasing, true);
    painter->setRenderHint(QPainter::SmoothPixmapTransform, true);

    drawBackground(*painter);

    if (m_showOverload) {
        drawClippedIndicator(*painter);
    }

    drawMeterBar(*painter);
}

void HorizontalVolumePressureMeterItem::setShowOverload(bool v)
{
    if (m_showOverload == v) {
        return;
    }
    m_showOverload = v;
    emit showOverloadChanged();
    update();
}

void HorizontalVolumePressureMeterItem::setOverloadWidth(int v)
{
    if (m_overloadWidth == v) {
        return;
    }
    m_overloadWidth = v;
    emit overloadWidthChanged();
    emit overloadTotalSpaceChanged();
    update();
}

void HorizontalVolumePressureMeterItem::setOverloadSpacing(int v)
{
    if (m_overloadSpacing == v) {
        return;
    }
    m_overloadSpacing = v;
    emit overloadSpacingChanged();
    emit overloadTotalSpaceChanged();
    update();
}

bool HorizontalVolumePressureMeterItem::isClipping() const
{
    return currentVolumePressure() >= maxDisplayedVolumePressure();
}

qreal HorizontalVolumePressureMeterItem::indicatorWidth() const
{
    return width() - overloadTotalSpace();
}

qreal HorizontalVolumePressureMeterItem::sampleValueToWidth(qreal sampleValue) const
{
    if (!meterModel()) {
        return 0.0;
    }
    return std::floor(indicatorWidth() * meterModel()->sampleToPosition(sampleValue));
}

void HorizontalVolumePressureMeterItem::drawBackground(QPainter& p) const
{
    p.fillRect(QRectF(0, 0, indicatorWidth(), height()), meterBackgroundColor());
}

void HorizontalVolumePressureMeterItem::drawClippedIndicator(QPainter& p) const
{
    const QColor color = clipped() ? clippedColor() : noClippedColor();
    p.fillRect(QRectF(indicatorWidth() + m_overloadSpacing, 0, m_overloadWidth, height()), color);
}

void HorizontalVolumePressureMeterItem::drawMeterBar(QPainter& p)
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

void HorizontalVolumePressureMeterItem::drawBarStyleDefault(QPainter& p)
{
    if (isClipping()) {
        p.fillRect(QRectF(0, 0, indicatorWidth(), height()), clippedColor());
        return;
    }
    const qreal w = sampleValueToWidth(currentVolumePressure());
    if (w > 0) {
        p.fillRect(QRectF(0, 0, w, height()), defaultColor());
    }
    drawPeakMarkers(p);
}

void HorizontalVolumePressureMeterItem::drawBarStyleRMS(QPainter& p)
{
    if (isClipping()) {
        p.fillRect(QRectF(0, 0, indicatorWidth(), height()), clippedColor());
        return;
    }
    const qreal xRMS = sampleValueToWidth(currentRMS());
    const qreal xPeak = sampleValueToWidth(currentVolumePressure());

    p.fillRect(QRectF(0, 0, xPeak, height()), rmsColor());
    p.fillRect(QRectF(xRMS, 0, xPeak - xRMS, height()), rmsOverlayColor());

    drawPeakMarkers(p);
}

void HorizontalVolumePressureMeterItem::drawBarStyleGradient(QPainter& p)
{
    const qreal w = sampleValueToWidth(currentVolumePressure());
    if (w > 0) {
        QLinearGradient grad(0, 0, indicatorWidth(), 0);
        grad.setColorAt(0.0, gradientColorGreen());
        grad.setColorAt(0.8, gradientColorYellow());
        grad.setColorAt(1.0, gradientColorRed());
        p.fillRect(QRectF(0, 0, w, height()), grad);
    }
    drawPeakMarkers(p);
}

void HorizontalVolumePressureMeterItem::drawPeakMarkers(QPainter& p)
{
    const qreal recentX = sampleValueToWidth(recentPeak());
    if (recentX > 0) {
        p.fillRect(QRectF(recentX, 0, 1, height()), recentPeakMarkerColor());
    }
    const qreal maxX = sampleValueToWidth(maxPeak());
    if (maxX > 0) {
        p.fillRect(QRectF(maxX, 0, 1, height()), maxPeakMarkerColor());
    }
}
}
