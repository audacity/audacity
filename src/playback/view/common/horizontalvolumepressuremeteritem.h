/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "volumepressuremeteritembase.h"

namespace au::playback {
class HorizontalVolumePressureMeterItem : public VolumePressureMeterItemBase
{
    Q_OBJECT

    Q_PROPERTY(bool showOverload READ showOverload WRITE setShowOverload NOTIFY showOverloadChanged)
    Q_PROPERTY(int overloadWidth READ overloadWidth WRITE setOverloadWidth NOTIFY overloadWidthChanged)
    Q_PROPERTY(int overloadSpacing READ overloadSpacing WRITE setOverloadSpacing NOTIFY overloadSpacingChanged)
    Q_PROPERTY(int overloadTotalSpace READ overloadTotalSpace NOTIFY overloadTotalSpaceChanged)

public:
    explicit HorizontalVolumePressureMeterItem(QQuickItem* parent = nullptr);

    void paint(QPainter* painter) override;

    bool showOverload() const { return m_showOverload; }
    void setShowOverload(bool v);

    int overloadWidth() const { return m_overloadWidth; }
    void setOverloadWidth(int v);

    int overloadSpacing() const { return m_overloadSpacing; }
    void setOverloadSpacing(int v);

    int overloadTotalSpace() const { return m_overloadWidth + m_overloadSpacing; }

signals:
    void showOverloadChanged();
    void overloadWidthChanged();
    void overloadSpacingChanged();
    void overloadTotalSpaceChanged();

protected:
    bool isClipping() const override;
    qreal recentPeakFloor() const override { return -60.0; }

private:
    qreal indicatorWidth() const;
    qreal sampleValueToWidth(qreal sampleValue) const;

    void drawBackground(QPainter& p) const;
    void drawClippedIndicator(QPainter& p) const;
    void drawMeterBar(QPainter& p);
    void drawBarStyleDefault(QPainter& p);
    void drawBarStyleRMS(QPainter& p);
    void drawBarStyleGradient(QPainter& p);
    void drawPeakMarkers(QPainter& p);

    bool m_showOverload = true;
    int m_overloadWidth = 6;
    int m_overloadSpacing = 2;
};
}
