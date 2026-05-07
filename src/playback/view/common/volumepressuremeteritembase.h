/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QColor>
#include <QPointer>
#include <QQuickPaintedItem>

#include <vector>

#include "metermodel.h"

namespace au::playback {
class VolumePressureMeterItemBase : public QQuickPaintedItem
{
    Q_OBJECT

    Q_PROPERTY(au::playback::MeterModel * meterModel READ meterModel WRITE setMeterModel NOTIFY meterModelChanged)

    Q_PROPERTY(qreal currentVolumePressure READ currentVolumePressure WRITE setCurrentVolumePressure NOTIFY currentVolumePressureChanged)
    Q_PROPERTY(qreal currentRMS READ currentRMS WRITE setCurrentRMS NOTIFY currentRMSChanged)
    Q_PROPERTY(qreal minDisplayedVolumePressure READ minDisplayedVolumePressure WRITE setMinDisplayedVolumePressure NOTIFY
               minDisplayedVolumePressureChanged)
    Q_PROPERTY(qreal maxDisplayedVolumePressure READ maxDisplayedVolumePressure WRITE setMaxDisplayedVolumePressure NOTIFY
               maxDisplayedVolumePressureChanged)

    Q_PROPERTY(int recentPeakIntervalMilliseconds READ recentPeakIntervalMilliseconds WRITE setRecentPeakIntervalMilliseconds NOTIFY
               recentPeakIntervalMillisecondsChanged)

    Q_PROPERTY(QColor clippedColor READ clippedColor WRITE setClippedColor NOTIFY clippedColorChanged)
    Q_PROPERTY(QColor noClippedColor READ noClippedColor WRITE setNoClippedColor NOTIFY noClippedColorChanged)
    Q_PROPERTY(QColor rmsColor READ rmsColor WRITE setRmsColor NOTIFY rmsColorChanged)
    Q_PROPERTY(QColor rmsOverlayColor READ rmsOverlayColor WRITE setRmsOverlayColor NOTIFY rmsOverlayColorChanged)
    Q_PROPERTY(QColor defaultColor READ defaultColor WRITE setDefaultColor NOTIFY defaultColorChanged)
    Q_PROPERTY(QColor gradientColorGreen READ gradientColorGreen WRITE setGradientColorGreen NOTIFY gradientColorGreenChanged)
    Q_PROPERTY(QColor gradientColorYellow READ gradientColorYellow WRITE setGradientColorYellow NOTIFY gradientColorYellowChanged)
    Q_PROPERTY(QColor gradientColorRed READ gradientColorRed WRITE setGradientColorRed NOTIFY gradientColorRedChanged)
    Q_PROPERTY(QColor meterBackgroundColor READ meterBackgroundColor WRITE setMeterBackgroundColor NOTIFY meterBackgroundColorChanged)
    Q_PROPERTY(QColor maxPeakMarkerColor READ maxPeakMarkerColor WRITE setMaxPeakMarkerColor NOTIFY maxPeakMarkerColorChanged)

public:
    explicit VolumePressureMeterItemBase(QQuickItem* parent = nullptr);

    Q_INVOKABLE void reset();
    Q_INVOKABLE void resetClipped();

    void paint(QPainter* painter) override = 0;

    MeterModel* meterModel() const { return m_meterModel; }
    void setMeterModel(MeterModel* model);

    qreal currentVolumePressure() const { return m_currentVolumePressure; }
    void setCurrentVolumePressure(qreal v);

    qreal currentRMS() const { return m_currentRMS; }
    void setCurrentRMS(qreal v);

    qreal minDisplayedVolumePressure() const { return m_minDisplayedVolumePressure; }
    void setMinDisplayedVolumePressure(qreal v);

    qreal maxDisplayedVolumePressure() const { return m_maxDisplayedVolumePressure; }
    void setMaxDisplayedVolumePressure(qreal v);

    int recentPeakIntervalMilliseconds() const { return m_recentPeakIntervalMs; }
    void setRecentPeakIntervalMilliseconds(int v);

    QColor clippedColor() const { return m_clippedColor; }
    void setClippedColor(const QColor& c);

    QColor noClippedColor() const { return m_noClippedColor; }
    void setNoClippedColor(const QColor& c);

    QColor rmsColor() const { return m_rmsColor; }
    void setRmsColor(const QColor& c);

    QColor rmsOverlayColor() const { return m_rmsOverlayColor; }
    void setRmsOverlayColor(const QColor& c);

    QColor defaultColor() const { return m_defaultColor; }
    void setDefaultColor(const QColor& c);

    QColor gradientColorGreen() const { return m_gradientColorGreen; }
    void setGradientColorGreen(const QColor& c);

    QColor gradientColorYellow() const { return m_gradientColorYellow; }
    void setGradientColorYellow(const QColor& c);

    QColor gradientColorRed() const { return m_gradientColorRed; }
    void setGradientColorRed(const QColor& c);

    QColor meterBackgroundColor() const { return m_meterBackgroundColor; }
    void setMeterBackgroundColor(const QColor& c);

    QColor maxPeakMarkerColor() const { return m_maxPeakMarkerColor; }
    void setMaxPeakMarkerColor(const QColor& c);

signals:
    void meterModelChanged();
    void currentVolumePressureChanged();
    void currentRMSChanged();
    void minDisplayedVolumePressureChanged();
    void maxDisplayedVolumePressureChanged();
    void recentPeakIntervalMillisecondsChanged();
    void clippedColorChanged();
    void noClippedColorChanged();
    void rmsColorChanged();
    void rmsOverlayColorChanged();
    void defaultColorChanged();
    void gradientColorGreenChanged();
    void gradientColorYellowChanged();
    void gradientColorRedChanged();
    void meterBackgroundColorChanged();
    void maxPeakMarkerColorChanged();

protected:
    qreal updatedVolumePressure() const { return m_updatedVolumePressure; }
    qreal maxPeak() const { return m_maxPeak; }
    qreal recentPeak() const { return m_recentPeak; }
    bool clipped() const { return m_clipped; }
    QColor recentPeakMarkerColor() const;

    virtual bool isClipping() const = 0;
    virtual qreal recentPeakFloor() const = 0;

private:
    void updateRecentPeak();

    QPointer<MeterModel> m_meterModel;

    qreal m_currentVolumePressure = -145.0;
    qreal m_currentRMS = -145.0;
    qreal m_minDisplayedVolumePressure = -60.0;
    qreal m_maxDisplayedVolumePressure = 0.0;

    int m_recentPeakIntervalMs = 600;

    qreal m_updatedVolumePressure = -145.0;
    qreal m_maxPeak = -145.0;
    qreal m_recentPeak = -145.0;
    bool m_clipped = false;

    struct PeakSample {
        qreal value;
        qint64 time;
    };
    std::vector<PeakSample> m_recentVolumePressure;

    QColor m_clippedColor;
    QColor m_noClippedColor;
    QColor m_rmsColor;
    QColor m_rmsOverlayColor;
    QColor m_defaultColor;
    QColor m_gradientColorGreen;
    QColor m_gradientColorYellow;
    QColor m_gradientColorRed;
    QColor m_meterBackgroundColor;
    QColor m_maxPeakMarkerColor;
};
}
