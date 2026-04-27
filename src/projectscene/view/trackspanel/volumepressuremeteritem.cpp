/*
* Audacity: A Digital Audio Editor
*/
#include "volumepressuremeteritem.h"

#include <QDateTime>
#include <QLinearGradient>
#include <QPainter>
#include <QPainterPath>

#include <algorithm>
#include <cmath>

#include "playback/playbacktypes.h"
#include "playback/view/common/metermodel.h"

using au::playback::MeterModel;
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
    : QQuickPaintedItem(parent)
{
    setAntialiasing(true);
}

void VolumePressureMeterItem::reset()
{
    m_maxPeak = -145.0;
    m_recentPeak = -145.0;
    m_updatedVolumePressure = -145.0;
    m_recentVolumePressure.clear();
    update();
}

void VolumePressureMeterItem::resetClipped()
{
    if (!m_clipped) {
        return;
    }
    m_clipped = false;
    update();
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

void VolumePressureMeterItem::setMeterModel(MeterModel* model)
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

void VolumePressureMeterItem::setCurrentVolumePressure(qreal v)
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

void VolumePressureMeterItem::setCurrentRMS(qreal v)
{
    if (m_currentRMS == v) {
        return;
    }
    m_currentRMS = v;
    emit currentRMSChanged();
    update();
}

void VolumePressureMeterItem::setMinDisplayedVolumePressure(qreal v)
{
    if (m_minDisplayedVolumePressure == v) {
        return;
    }
    m_minDisplayedVolumePressure = v;
    emit minDisplayedVolumePressureChanged();
    update();
}

void VolumePressureMeterItem::setMaxDisplayedVolumePressure(qreal v)
{
    if (m_maxDisplayedVolumePressure == v) {
        return;
    }
    m_maxDisplayedVolumePressure = v;
    emit maxDisplayedVolumePressureChanged();
    update();
}

void VolumePressureMeterItem::setRecentPeakIntervalMiliseconds(int v)
{
    if (m_recentPeakIntervalMs == v) {
        return;
    }
    m_recentPeakIntervalMs = v;
    emit recentPeakIntervalMilisecondsChanged();
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

void VolumePressureMeterItem::setClippedColor(const QColor& c)
{
    if (m_clippedColor == c) {
        return;
    }
    m_clippedColor = c;
    emit clippedColorChanged();
    update();
}

void VolumePressureMeterItem::setNoClippedColor(const QColor& c)
{
    if (m_noClippedColor == c) {
        return;
    }
    m_noClippedColor = c;
    emit noClippedColorChanged();
    update();
}

void VolumePressureMeterItem::setRmsColor(const QColor& c)
{
    if (m_rmsColor == c) {
        return;
    }
    m_rmsColor = c;
    emit rmsColorChanged();
    update();
}

void VolumePressureMeterItem::setRmsOverlayColor(const QColor& c)
{
    if (m_rmsOverlayColor == c) {
        return;
    }
    m_rmsOverlayColor = c;
    emit rmsOverlayColorChanged();
    update();
}

void VolumePressureMeterItem::setDefaultColor(const QColor& c)
{
    if (m_defaultColor == c) {
        return;
    }
    m_defaultColor = c;
    emit defaultColorChanged();
    update();
}

void VolumePressureMeterItem::setGradientColorGreen(const QColor& c)
{
    if (m_gradientColorGreen == c) {
        return;
    }
    m_gradientColorGreen = c;
    emit gradientColorGreenChanged();
    update();
}

void VolumePressureMeterItem::setGradientColorYellow(const QColor& c)
{
    if (m_gradientColorYellow == c) {
        return;
    }
    m_gradientColorYellow = c;
    emit gradientColorYellowChanged();
    update();
}

void VolumePressureMeterItem::setGradientColorRed(const QColor& c)
{
    if (m_gradientColorRed == c) {
        return;
    }
    m_gradientColorRed = c;
    emit gradientColorRedChanged();
    update();
}

void VolumePressureMeterItem::setMeterBackgroundColor(const QColor& c)
{
    if (m_meterBackgroundColor == c) {
        return;
    }
    m_meterBackgroundColor = c;
    emit meterBackgroundColorChanged();
    update();
}

void VolumePressureMeterItem::setMaxPeakMarkerColor(const QColor& c)
{
    if (m_maxPeakMarkerColor == c) {
        return;
    }
    m_maxPeakMarkerColor = c;
    emit maxPeakMarkerColorChanged();
    update();
}

bool VolumePressureMeterItem::isClipping() const
{
    return m_updatedVolumePressure >= m_maxDisplayedVolumePressure;
}

qreal VolumePressureMeterItem::indicatorHeight() const
{
    return height() - m_overloadHeight;
}

qreal VolumePressureMeterItem::sampleValueToHeight(qreal sampleValue) const
{
    if (!m_meterModel) {
        return 0.0;
    }
    return indicatorHeight() * m_meterModel->sampleToPosition(sampleValue);
}

void VolumePressureMeterItem::updateRecentPeak()
{
    const qint64 now = QDateTime::currentMSecsSinceEpoch();
    m_recentVolumePressure.push_back({ m_updatedVolumePressure, now });

    const qint64 cutoff = now - m_recentPeakIntervalMs;
    auto firstKeep = std::find_if(m_recentVolumePressure.begin(), m_recentVolumePressure.end(),
                                  [cutoff](const PeakSample& s) { return s.time >= cutoff; });
    m_recentVolumePressure.erase(m_recentVolumePressure.begin(), firstKeep);

    qreal peak = -145.0;
    for (const auto& s : m_recentVolumePressure) {
        peak = std::max(peak, s.value);
    }
    m_recentPeak = peak;
}

QColor VolumePressureMeterItem::recentPeakMarkerColor() const
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

void VolumePressureMeterItem::drawBackground(QPainter& p) const
{
    QPainterPath path;
    buildRoundedRectPath(path, 0, 0, m_indicatorWidth, height(), 2, true, true);
    p.fillPath(path, m_meterBackgroundColor);
}

void VolumePressureMeterItem::drawClippedIndicator(QPainter& p) const
{
    const QColor color = m_clipped ? m_clippedColor : m_noClippedColor;
    QPainterPath path;
    buildRoundedRectPath(path, 0, 0, m_indicatorWidth, m_overloadHeight, 2, true, false);
    p.fillPath(path, color);
}

void VolumePressureMeterItem::drawMeterBar(QPainter& p)
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

void VolumePressureMeterItem::drawBarStyleDefault(QPainter& p)
{
    if (isClipping()) {
        p.fillRect(QRectF(0, 0, m_indicatorWidth, indicatorHeight() + m_overloadHeight), m_clippedColor);
        return;
    }
    const qreal h = sampleValueToHeight(m_updatedVolumePressure);
    if (h > 0) {
        fillRoundedRect(p, m_defaultColor, 0, height() - h, m_indicatorWidth, h, 2, false, true);
    }
    drawPeakMarkers(p);
}

void VolumePressureMeterItem::drawBarStyleRMS(QPainter& p)
{
    if (isClipping()) {
        p.fillRect(QRectF(0, 0, m_indicatorWidth, indicatorHeight() + m_overloadHeight), m_clippedColor);
        return;
    }
    const qreal yRMS = sampleValueToHeight(m_currentRMS);
    const qreal yPeak = sampleValueToHeight(m_currentVolumePressure);

    if (yRMS > 0) {
        fillRoundedRect(p, m_rmsColor, 0, height() - yPeak, m_indicatorWidth, yPeak, 2, false, true);
        p.fillRect(QRectF(0, height() - yPeak, m_indicatorWidth, yPeak - yRMS), m_rmsOverlayColor);
    }

    drawPeakMarkers(p);
}

void VolumePressureMeterItem::drawBarStyleGradient(QPainter& p)
{
    const qreal h = sampleValueToHeight(m_updatedVolumePressure);
    if (h > 0) {
        QLinearGradient grad(0, 0, 0, indicatorHeight());
        grad.setColorAt(0.0, m_gradientColorRed);
        grad.setColorAt(0.2, m_gradientColorYellow);
        grad.setColorAt(1.0, m_gradientColorGreen);
        fillRoundedRect(p, grad, 0, height() - h, m_indicatorWidth, h, 2, false, true);
    }
    drawPeakMarkers(p);
}

void VolumePressureMeterItem::drawPeakMarkers(QPainter& p)
{
    const qreal recentH = sampleValueToHeight(m_recentPeak);
    if (recentH > 0) {
        p.fillRect(QRectF(0, height() - recentH, m_indicatorWidth, 1), recentPeakMarkerColor());
    }
    const qreal maxH = sampleValueToHeight(m_maxPeak);
    if (maxH > 0) {
        p.fillRect(QRectF(0, height() - maxH, m_indicatorWidth, 1), m_maxPeakMarkerColor);
    }
}
}
