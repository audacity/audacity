/*
* Audacity: A Digital Audio Editor
*/
#include "horizontalvolumepressuremeteritem.h"

#include <QDateTime>
#include <QLinearGradient>
#include <QPainter>

#include <algorithm>
#include <cmath>

#include "metermodel.h"
#include "playbacktypes.h"

namespace au::playback {
HorizontalVolumePressureMeterItem::HorizontalVolumePressureMeterItem(QQuickItem* parent)
    : QQuickPaintedItem(parent)
{
    setAntialiasing(true);
}

void HorizontalVolumePressureMeterItem::reset()
{
    m_maxPeak = -145.0;
    m_recentPeak = -145.0;
    m_recentVolumePressure.clear();
    m_updatedVolumePressure = -145.0;
    update();
}

void HorizontalVolumePressureMeterItem::resetClipped()
{
    if (!m_clipped) {
        return;
    }
    m_clipped = false;
    update();
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

void HorizontalVolumePressureMeterItem::setMeterModel(MeterModel* model)
{
    if (m_meterModel == model) {
        return;
    }
    if (m_meterModel) {
        disconnect(m_meterModel, nullptr, this, nullptr);
    }
    m_meterModel = model;
    if (m_meterModel) {
        const auto repaint = [this]() { update(); };
        connect(m_meterModel, &MeterModel::smallStepsChanged, this, repaint);
        connect(m_meterModel, &MeterModel::dbRangeChanged, this, repaint);
        connect(m_meterModel, &MeterModel::meterStyleChanged, this, repaint);
        connect(m_meterModel, &MeterModel::meterDbRangeChanged, this, repaint);
    }
    emit meterModelChanged();
    update();
}

void HorizontalVolumePressureMeterItem::setCurrentVolumePressure(qreal v)
{
    if (m_currentVolumePressure == v) {
        return;
    }
    m_currentVolumePressure = v;
    emit currentVolumePressureChanged();

    if (!std::isnan(v)) {
        m_maxPeak = std::max(m_maxPeak, v);
        m_updatedVolumePressure = v;
        updateRecentPeak();
    }
    if (isClipping() && !m_clipped) {
        m_clipped = true;
    }
    update();
}

void HorizontalVolumePressureMeterItem::setCurrentRMS(qreal v)
{
    if (m_currentRMS == v) {
        return;
    }
    m_currentRMS = v;
    emit currentRMSChanged();
    update();
}

void HorizontalVolumePressureMeterItem::setMinDisplayedVolumePressure(qreal v)
{
    if (m_minDisplayedVolumePressure == v) {
        return;
    }
    m_minDisplayedVolumePressure = v;
    emit minDisplayedVolumePressureChanged();
    update();
}

void HorizontalVolumePressureMeterItem::setMaxDisplayedVolumePressure(qreal v)
{
    if (m_maxDisplayedVolumePressure == v) {
        return;
    }
    m_maxDisplayedVolumePressure = v;
    emit maxDisplayedVolumePressureChanged();
    update();
}

void HorizontalVolumePressureMeterItem::setRecentPeakIntervalMiliseconds(int v)
{
    if (m_recentPeakIntervalMs == v) {
        return;
    }
    m_recentPeakIntervalMs = v;
    emit recentPeakIntervalMilisecondsChanged();
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

void HorizontalVolumePressureMeterItem::setClippedColor(const QColor& c)
{
    if (m_clippedColor == c) {
        return;
    }
    m_clippedColor = c;
    emit clippedColorChanged();
    update();
}

void HorizontalVolumePressureMeterItem::setNoClippedColor(const QColor& c)
{
    if (m_noClippedColor == c) {
        return;
    }
    m_noClippedColor = c;
    emit noClippedColorChanged();
    update();
}

void HorizontalVolumePressureMeterItem::setRmsColor(const QColor& c)
{
    if (m_rmsColor == c) {
        return;
    }
    m_rmsColor = c;
    emit rmsColorChanged();
    update();
}

void HorizontalVolumePressureMeterItem::setRmsOverlayColor(const QColor& c)
{
    if (m_rmsOverlayColor == c) {
        return;
    }
    m_rmsOverlayColor = c;
    emit rmsOverlayColorChanged();
    update();
}

void HorizontalVolumePressureMeterItem::setDefaultColor(const QColor& c)
{
    if (m_defaultColor == c) {
        return;
    }
    m_defaultColor = c;
    emit defaultColorChanged();
    update();
}

void HorizontalVolumePressureMeterItem::setGradientColorGreen(const QColor& c)
{
    if (m_gradientColorGreen == c) {
        return;
    }
    m_gradientColorGreen = c;
    emit gradientColorGreenChanged();
    update();
}

void HorizontalVolumePressureMeterItem::setGradientColorYellow(const QColor& c)
{
    if (m_gradientColorYellow == c) {
        return;
    }
    m_gradientColorYellow = c;
    emit gradientColorYellowChanged();
    update();
}

void HorizontalVolumePressureMeterItem::setGradientColorRed(const QColor& c)
{
    if (m_gradientColorRed == c) {
        return;
    }
    m_gradientColorRed = c;
    emit gradientColorRedChanged();
    update();
}

void HorizontalVolumePressureMeterItem::setMeterBackgroundColor(const QColor& c)
{
    if (m_meterBackgroundColor == c) {
        return;
    }
    m_meterBackgroundColor = c;
    emit meterBackgroundColorChanged();
    update();
}

void HorizontalVolumePressureMeterItem::setMaxPeakMarkerColor(const QColor& c)
{
    if (m_maxPeakMarkerColor == c) {
        return;
    }
    m_maxPeakMarkerColor = c;
    emit maxPeakMarkerColorChanged();
    update();
}

bool HorizontalVolumePressureMeterItem::isClipping() const
{
    return m_currentVolumePressure >= m_maxDisplayedVolumePressure;
}

qreal HorizontalVolumePressureMeterItem::indicatorWidth() const
{
    return width() - overloadTotalSpace();
}

qreal HorizontalVolumePressureMeterItem::sampleValueToWidth(qreal sampleValue) const
{
    if (!m_meterModel) {
        return 0.0;
    }
    return std::floor(indicatorWidth() * m_meterModel->sampleToPosition(sampleValue));
}

void HorizontalVolumePressureMeterItem::updateRecentPeak()
{
    const qint64 now = QDateTime::currentMSecsSinceEpoch();
    m_recentVolumePressure.push_back({ m_updatedVolumePressure, now });

    const qint64 cutoff = now - m_recentPeakIntervalMs;
    auto firstKeep = std::find_if(m_recentVolumePressure.begin(), m_recentVolumePressure.end(),
                                  [cutoff](const PeakSample& s) { return s.time >= cutoff; });
    m_recentVolumePressure.erase(m_recentVolumePressure.begin(), firstKeep);

    qreal peak = -60.0;
    for (const auto& s : m_recentVolumePressure) {
        peak = std::max(peak, s.value);
    }
    m_recentPeak = peak;
}

QColor HorizontalVolumePressureMeterItem::recentPeakMarkerColor() const
{
    if (!m_meterModel) {
        return m_maxPeakMarkerColor;
    }
    switch (m_meterModel->meterStyle()) {
    case PlaybackMeterStyle::MeterStyle::Default:
        return m_defaultColor;
    case PlaybackMeterStyle::MeterStyle::RMS:
        return m_rmsColor;
    case PlaybackMeterStyle::MeterStyle::Gradient: {
        const qreal range = m_maxDisplayedVolumePressure - m_minDisplayedVolumePressure;
        if (range <= 0.0) {
            return m_maxPeakMarkerColor;
        }
        const qreal ratio = (m_recentPeak - m_minDisplayedVolumePressure) / range;
        if (ratio < 0.2) {
            return m_gradientColorGreen;
        }
        if (ratio < 0.8) {
            return m_gradientColorYellow;
        }
        return m_gradientColorRed;
    }
    }
    return m_maxPeakMarkerColor;
}

void HorizontalVolumePressureMeterItem::drawBackground(QPainter& p) const
{
    p.fillRect(QRectF(0, 0, indicatorWidth(), height()), m_meterBackgroundColor);
}

void HorizontalVolumePressureMeterItem::drawClippedIndicator(QPainter& p) const
{
    const QColor color = m_clipped ? m_clippedColor : m_noClippedColor;
    p.fillRect(QRectF(indicatorWidth() + m_overloadSpacing, 0, m_overloadWidth, height()), color);
}

void HorizontalVolumePressureMeterItem::drawMeterBar(QPainter& p)
{
    if (!m_meterModel) {
        return;
    }
    switch (m_meterModel->meterStyle()) {
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
        p.fillRect(QRectF(0, 0, indicatorWidth(), height()), m_clippedColor);
        return;
    }
    const qreal w = sampleValueToWidth(m_currentVolumePressure);
    if (w > 0) {
        p.fillRect(QRectF(0, 0, w, height()), m_defaultColor);
    }
    drawPeakMarkers(p);
}

void HorizontalVolumePressureMeterItem::drawBarStyleRMS(QPainter& p)
{
    if (isClipping()) {
        p.fillRect(QRectF(0, 0, indicatorWidth(), height()), m_clippedColor);
        return;
    }
    const qreal xRMS = sampleValueToWidth(m_currentRMS);
    const qreal xPeak = sampleValueToWidth(m_currentVolumePressure);

    p.fillRect(QRectF(0, 0, xPeak, height()), m_rmsColor);
    p.fillRect(QRectF(xRMS, 0, xPeak - xRMS, height()), m_rmsOverlayColor);

    drawPeakMarkers(p);
}

void HorizontalVolumePressureMeterItem::drawBarStyleGradient(QPainter& p)
{
    const qreal w = sampleValueToWidth(m_currentVolumePressure);
    if (w > 0) {
        QLinearGradient grad(0, 0, indicatorWidth(), 0);
        grad.setColorAt(0.0, m_gradientColorGreen);
        grad.setColorAt(0.8, m_gradientColorYellow);
        grad.setColorAt(1.0, m_gradientColorRed);
        p.fillRect(QRectF(0, 0, w, height()), grad);
    }
    drawPeakMarkers(p);
}

void HorizontalVolumePressureMeterItem::drawPeakMarkers(QPainter& p)
{
    const qreal recentX = sampleValueToWidth(m_recentPeak);
    if (recentX > 0) {
        p.fillRect(QRectF(recentX, 0, 1, height()), recentPeakMarkerColor());
    }
    const qreal maxX = sampleValueToWidth(m_maxPeak);
    if (maxX > 0) {
        p.fillRect(QRectF(maxX, 0, 1, height()), m_maxPeakMarkerColor);
    }
}
}
