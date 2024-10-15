/*
* Audacity: A Digital Audio Editor
*/

#include "gridlines.h"

using namespace au::projectscene;

GridLines::GridLines(QQuickItem* parent)
{
}

void GridLines::paint(QPainter* painter)
{
    // set background
    QRectF rect = boundingRect();
    QColor canvasColor = uiconfiguration()->currentTheme().values.value(muse::ui::BACKGROUND_TERTIARY_COLOR).toString();
    painter->fillRect(rect, canvasColor);

    // draw left border
    const qreal h = height();
    QPen pen = painter->pen();
    pen.setWidth(2);
    QColor beatStrokeColor = uiconfiguration()->currentTheme().values.value(muse::ui::WHITE_COLOR).toString();
    beatStrokeColor.setAlpha(static_cast<int>(0.1 * 256));
    pen.setColor(beatStrokeColor);
    painter->setPen(pen);
    painter->drawLine(QLineF(0, 0, 0, h));

    // draw gridlines
    drawGridLines(painter);

    // draw zebra highlighting (only for beats and measures)
    if (configuration()->timelineRulerMode() == TimelineRulerMode::BEATS_AND_MEASURES) {
        drawZebraHighlighting(painter);
    }
}

TimelineRuler* GridLines::timelineRuler() const
{
    return m_timelineRuler;
}

void GridLines::setTimelineRuler(TimelineRuler* newTimelineRuler)
{
    if (m_timelineRuler == newTimelineRuler) {
        return;
    }

    if (m_timelineRuler) {
        disconnect(m_timelineRuler, nullptr, this, nullptr);
    }

    m_timelineRuler = newTimelineRuler;

    if (m_timelineRuler) {
        auto updateView = [this] (Ticks ticks) {
            m_ticks = ticks;
            update();
        };

        connect(m_timelineRuler, &TimelineRuler::ticksChanged, this, updateView);
    }
}

void GridLines::drawGridLines(QPainter* painter)
{
    const qreal h = height();
    QColor beatStrokeColor = uiconfiguration()->currentTheme().values.value(muse::ui::WHITE_COLOR).toString();
    beatStrokeColor.setAlpha(static_cast<int>(0.06 * 256));
    QColor majorBeatStrokeColor = uiconfiguration()->currentTheme().values.value(muse::ui::WHITE_COLOR).toString();
    majorBeatStrokeColor.setAlpha(static_cast<int>(0.1 * 256));

    QPen pen = painter->pen();
    pen.setWidth(1);

    for (const auto& tick : m_ticks) {
        double x = tick.line.p1().x();

        if (tick.tickType == TickType::MAJOR) {
            pen.setColor(majorBeatStrokeColor);
        } else {
            pen.setColor(beatStrokeColor);
        }
        painter->setPen(pen);
        painter->drawLine(QLineF(x, 0, x, h));
    }
}

void GridLines::drawZebraHighlighting(QPainter* painter)
{
    if (m_ticks.isEmpty()) {
        return;
    }

    const qreal h = height();
    const qreal w = width();
    QColor highlightColor = uiconfiguration()->currentTheme().values.value(muse::ui::WHITE_COLOR).toString();
    highlightColor.setAlpha(static_cast<int>(0.02 * 256));

    int BPM = m_timelineRuler->timelineContext()->BPM();
    double SPB = static_cast<double>(60) / BPM;
    IntervalInfo intervalInfo = m_timelineRuler->intervalInfo();

    // highlight only when displaying at zoom between 1/4 and 1/32 ticks
    if (intervalInfo.minorMinor >= 2 * SPB || intervalInfo.minorMinor <= (SPB / 16)) {
        return;
    }

    int highlightStart = 0;
    int highlightEnd = 0;
    bool pendingDraw = false;
    const double tolerance = 0.01; // Tolerance for floating-point comparison

    for (auto i = 0; i < m_ticks.size(); ++i) {
        auto tick = m_ticks.at(i);
        double spbRemainder = std::abs(std::remainder(tick.timeValue, SPB));
        double doubledSpbRemainder = std::abs(std::remainder(tick.timeValue, SPB * 2));
        if (spbRemainder < tolerance && doubledSpbRemainder < tolerance) {
            highlightStart = tick.line.p1().x();
            pendingDraw = true;
        } else if (spbRemainder < tolerance && doubledSpbRemainder >= tolerance) {
            highlightEnd = tick.line.p1().x();
            QRectF rect = QRectF(QPointF(highlightStart, 0), QPointF(highlightEnd, h));
            painter->fillRect(rect, highlightColor);
            pendingDraw = false;
        }
    }

    if (pendingDraw) {
        QRectF rect = QRectF(QPointF(highlightStart, 0), QPointF(w, h));
        painter->fillRect(rect, highlightColor);
    }
}
