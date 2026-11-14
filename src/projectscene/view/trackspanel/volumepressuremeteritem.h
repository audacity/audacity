/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "playback/view/common/volumepressuremeteritembase.h"

namespace au::projectscene {
class VolumePressureMeterItem : public au::playback::VolumePressureMeterItemBase
{
    Q_OBJECT

    Q_PROPERTY(qreal indicatorWidth READ indicatorWidth WRITE setIndicatorWidth NOTIFY indicatorWidthChanged)
    Q_PROPERTY(bool showClippedInfo READ showClippedInfo WRITE setShowClippedInfo NOTIFY showClippedInfoChanged)
    Q_PROPERTY(int overloadHeight READ overloadHeight WRITE setOverloadHeight NOTIFY overloadHeightChanged)

public:
    explicit VolumePressureMeterItem(QQuickItem* parent = nullptr);

    void paint(QPainter* painter) override;

    qreal indicatorWidth() const { return m_indicatorWidth; }
    void setIndicatorWidth(qreal v);

    bool showClippedInfo() const { return m_showClippedInfo; }
    void setShowClippedInfo(bool v);

    int overloadHeight() const { return m_overloadHeight; }
    void setOverloadHeight(int v);

signals:
    void indicatorWidthChanged();
    void showClippedInfoChanged();
    void overloadHeightChanged();

protected:
    bool isClipping() const override;
    qreal recentPeakFloor() const override { return -145.0; }

private:
    qreal indicatorHeight() const;
    qreal sampleValueToHeight(qreal sampleValue) const;

    void drawBackground(QPainter& p) const;
    void drawClippedIndicator(QPainter& p) const;
    void drawMeterBar(QPainter& p);
    void drawBarStyleDefault(QPainter& p);
    void drawBarStyleRMS(QPainter& p);
    void drawBarStyleGradient(QPainter& p);
    void drawPeakMarkers(QPainter& p);

    qreal m_indicatorWidth = 0.0;
    bool m_showClippedInfo = true;
    int m_overloadHeight = 4;
};
}
