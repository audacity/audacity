/*
* Audacity: A Digital Audio Editor
*/
#include "volumepressuremeteritembase.h"

#include <QDateTime>

#include <algorithm>
#include <cmath>

#include "playbacktypes.h"

namespace au::playback {
VolumePressureMeterItemBase::VolumePressureMeterItemBase(QQuickItem* parent)
    : QQuickPaintedItem(parent)
{
    setAntialiasing(true);
}

void VolumePressureMeterItemBase::reset()
{
    m_maxPeak = -145.0;
    m_recentPeak = -145.0;
    m_updatedVolumePressure = -145.0;
    m_recentVolumePressure.clear();
    update();
}

void VolumePressureMeterItemBase::resetClipped()
{
    if (!m_clipped) {
        return;
    }
    m_clipped = false;
    update();
}

void VolumePressureMeterItemBase::setMeterModel(MeterModel* model)
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

void VolumePressureMeterItemBase::setCurrentVolumePressure(qreal v)
{
    if (m_currentVolumePressure != v) {
        m_currentVolumePressure = v;
        emit currentVolumePressureChanged();
    }

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

void VolumePressureMeterItemBase::setCurrentRMS(qreal v)
{
    if (m_currentRMS == v) {
        return;
    }
    m_currentRMS = v;
    emit currentRMSChanged();
    update();
}

void VolumePressureMeterItemBase::setMinDisplayedVolumePressure(qreal v)
{
    if (m_minDisplayedVolumePressure == v) {
        return;
    }
    m_minDisplayedVolumePressure = v;
    emit minDisplayedVolumePressureChanged();
    update();
}

void VolumePressureMeterItemBase::setMaxDisplayedVolumePressure(qreal v)
{
    if (m_maxDisplayedVolumePressure == v) {
        return;
    }
    m_maxDisplayedVolumePressure = v;
    emit maxDisplayedVolumePressureChanged();
    update();
}

void VolumePressureMeterItemBase::setRecentPeakIntervalMilliseconds(int v)
{
    if (m_recentPeakIntervalMs == v) {
        return;
    }
    m_recentPeakIntervalMs = v;
    emit recentPeakIntervalMillisecondsChanged();
    updateRecentPeak();
    update();
}

void VolumePressureMeterItemBase::setClippedColor(const QColor& c)
{
    if (m_clippedColor == c) {
        return;
    }
    m_clippedColor = c;
    emit clippedColorChanged();
    update();
}

void VolumePressureMeterItemBase::setNoClippedColor(const QColor& c)
{
    if (m_noClippedColor == c) {
        return;
    }
    m_noClippedColor = c;
    emit noClippedColorChanged();
    update();
}

void VolumePressureMeterItemBase::setRmsColor(const QColor& c)
{
    if (m_rmsColor == c) {
        return;
    }
    m_rmsColor = c;
    emit rmsColorChanged();
    update();
}

void VolumePressureMeterItemBase::setRmsOverlayColor(const QColor& c)
{
    if (m_rmsOverlayColor == c) {
        return;
    }
    m_rmsOverlayColor = c;
    emit rmsOverlayColorChanged();
    update();
}

void VolumePressureMeterItemBase::setDefaultColor(const QColor& c)
{
    if (m_defaultColor == c) {
        return;
    }
    m_defaultColor = c;
    emit defaultColorChanged();
    update();
}

void VolumePressureMeterItemBase::setGradientColorGreen(const QColor& c)
{
    if (m_gradientColorGreen == c) {
        return;
    }
    m_gradientColorGreen = c;
    emit gradientColorGreenChanged();
    update();
}

void VolumePressureMeterItemBase::setGradientColorYellow(const QColor& c)
{
    if (m_gradientColorYellow == c) {
        return;
    }
    m_gradientColorYellow = c;
    emit gradientColorYellowChanged();
    update();
}

void VolumePressureMeterItemBase::setGradientColorRed(const QColor& c)
{
    if (m_gradientColorRed == c) {
        return;
    }
    m_gradientColorRed = c;
    emit gradientColorRedChanged();
    update();
}

void VolumePressureMeterItemBase::setMeterBackgroundColor(const QColor& c)
{
    if (m_meterBackgroundColor == c) {
        return;
    }
    m_meterBackgroundColor = c;
    emit meterBackgroundColorChanged();
    update();
}

void VolumePressureMeterItemBase::setMaxPeakMarkerColor(const QColor& c)
{
    if (m_maxPeakMarkerColor == c) {
        return;
    }
    m_maxPeakMarkerColor = c;
    emit maxPeakMarkerColorChanged();
    update();
}

void VolumePressureMeterItemBase::updateRecentPeak()
{
    const qint64 now = QDateTime::currentMSecsSinceEpoch();
    m_recentVolumePressure.push_back({ m_updatedVolumePressure, now });

    const qint64 cutoff = now - m_recentPeakIntervalMs;
    auto firstKeep = std::find_if(m_recentVolumePressure.begin(), m_recentVolumePressure.end(),
                                  [cutoff](const PeakSample& s) { return s.time >= cutoff; });
    m_recentVolumePressure.erase(m_recentVolumePressure.begin(), firstKeep);

    qreal peak = recentPeakFloor();
    for (const auto& s : m_recentVolumePressure) {
        peak = std::max(peak, s.value);
    }
    m_recentPeak = peak;
}

QColor VolumePressureMeterItemBase::recentPeakMarkerColor() const
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
}
