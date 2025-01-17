#include <QtCore/QString>
#include <QtCore/QVector>
#include <QtGui/QColor>
#include <QtGui/QPainter>

#include "timelineruler.h"

namespace {
constexpr int MINORMINOR_TICK_HEIGHT_RATIO = 8;
constexpr int MINOR_TICK_HEIGHT_RATIO = 4;
constexpr int FONT_SIZE = 12;
constexpr double TICK_ALPHA_MAJOR = 1.0;
constexpr double TICK_ALPHA_MINOR = 0.5;
constexpr double LABEL_ALPHA_MAJOR = 1.0;
constexpr double LABEL_ALPHA_MINOR = 0.75;
constexpr int LABEL_OFFSET = 2;
}

using namespace au::projectscene;

TimelineRuler::TimelineRuler(QQuickItem* parent)
    : QQuickPaintedItem(parent)
{
    setFormatter(configuration()->timelineRulerMode());

    uiconfiguration()->currentThemeChanged().onNotify(this, [this]() { update(); });
    configuration()->timelineRulerModeChanged().onReceive(this, [this](const TimelineRulerMode mode){
        setFormatter(mode);
        update();
    });
}

bool TimelineRuler::isMajorSection(int y) const
{
    return y < (height() / 2);
}

IntervalInfo TimelineRuler::intervalInfo()
{
    return m_formatter->intervalInfo(m_context);
}

void TimelineRuler::setFormatter(const TimelineRulerMode mode)
{
    if (mode == TimelineRulerMode::MINUTES_AND_SECONDS) {
        m_formatter = std::make_unique<TimeFormat>();
    } else {
        m_formatter = std::make_unique<BeatsMeasuresFormat>();
    }
}

void TimelineRuler::paint(QPainter* painter)
{
    const qreal w = width();
    const qreal h = height();

    // determine current time interval and prepare ticks
    IntervalInfo interval = m_formatter->intervalInfo(m_context);
    Ticks ticks = prepareTickData(interval, w, h);

    // begin painting
    QPen pen = painter->pen();
    pen.setWidth(2);
    pen.setColor(uiconfiguration()->currentTheme().values.value(muse::ui::STROKE_COLOR).toString());
    painter->setPen(pen);

    // vertical line (ruler border)
    painter->drawLine(QLineF(0, 0, 0, h));

    pen.setWidth(1);
    painter->setPen(pen);
    // horizontal line in the middle
    painter->drawLine(QLineF(0, h / 2, w, h / 2));

    drawTicks(painter, ticks);
    drawLabels(painter, ticks, w, h);

    emit ticksChanged(ticks);
}

Ticks TimelineRuler::prepareTickData(const IntervalInfo& timeInterval, double w, double h)
{
    Ticks ticks;

    // start calculating 100px over left edge of the ruler
    double leftBuffer = 100;
    double value = m_context->frameStartTime() - leftBuffer / m_context->zoom();
    double x = -leftBuffer;

    // find value and position of the first tick
    double remainder = std::remainder(value, timeInterval.minorMinor);
    if (remainder != 0) {
        x += (timeInterval.minorMinor - remainder) * m_context->zoom();
        value += (timeInterval.minorMinor - remainder);
    }

    auto tickHeight = [&](TickType tickType) {
        switch (tickType) {
        case TickType::MAJOR: return h;
        case TickType::MINOR: return h / MINOR_TICK_HEIGHT_RATIO;
        case TickType::MINORMINOR: return h / MINORMINOR_TICK_HEIGHT_RATIO;
        }
        return 0.0;
    };

    while (x < w)
    {
        // determine tick type
        TickType tickType = determineTickType(value, timeInterval);
        QString tickLabel = m_formatter->label(value, timeInterval, tickType, m_context);
        QLineF tick = QLineF(x, h - 2, x, h - 1 - tickHeight(tickType));
        double labelPos = x + LABEL_OFFSET;

        if (tickType == TickType::MAJOR || tickType == TickType::MINOR) {
            // add tick with label
            ticks.append(TickInfo { labelPos, tickLabel, tickType, tick, value });
        } else {
            // add tick without label
            ticks.append(TickInfo { -1.0, QString(), tickType, tick, value });
        }

        x += m_context->zoom() * timeInterval.minorMinor;
        value += timeInterval.minorMinor;
    }

    return ticks;
}

TickType TimelineRuler::determineTickType(double value, const IntervalInfo& timeInterval)
{
    if (muse::is_zero(std::abs(std::remainder(value, timeInterval.major)))) {
        return TickType::MAJOR;
    } else if (muse::is_zero(std::abs(std::remainder(value, timeInterval.minor)))) {
        return TickType::MINOR;
    } else {
        return TickType::MINORMINOR;
    }
}

void TimelineRuler::drawLabels(QPainter* painter, const Ticks& ticks, double w, double h)
{
    QFont textFont;
    textFont.setPixelSize(FONT_SIZE);
    QPen pen = painter->pen();
    QColor labelColor = uiconfiguration()->currentTheme().values.value(muse::ui::FONT_PRIMARY_COLOR).toString();
    pen.setColor(labelColor);
    painter->setPen(pen);

    for (qsizetype i = 0; i < ticks.count(); i++) {
        if (ticks[i].x == -1.0) {
            continue;
        }
        labelColor.setAlphaF(ticks[i].tickType == TickType::MAJOR ? LABEL_ALPHA_MAJOR : LABEL_ALPHA_MINOR);
        pen.setColor(labelColor);
        painter->setPen(pen);
        textFont.setBold(ticks[i].tickType == TickType::MAJOR);
        painter->setFont(textFont);
        painter->drawText(QRectF(ticks[i].x, 0, w, h / 2), Qt::AlignLeft | Qt::AlignVCenter, ticks[i].tickLabel);
    }
}

void TimelineRuler::drawTicks(QPainter* painter, const Ticks& ticks)
{
    QPen pen = painter->pen();
    QColor majorTickColor = QColor(uiconfiguration()->currentTheme().values.value(muse::ui::STROKE_COLOR).toString());
    QColor minorTickColor = QColor(uiconfiguration()->currentTheme().values.value(muse::ui::FONT_PRIMARY_COLOR).toString());

    for (auto i = 0; i < ticks.count(); ++i) {
        QColor tickColor = ticks[i].tickType == TickType::MAJOR ? majorTickColor : minorTickColor;
        tickColor.setAlphaF(ticks[i].tickType == TickType::MAJOR ? TICK_ALPHA_MAJOR : TICK_ALPHA_MINOR);
        pen.setColor(tickColor);
        painter->setPen(pen);
        painter->drawLine(ticks[i].line);
    }
}

TimelineContext* TimelineRuler::timelineContext() const
{
    return m_context;
}

void TimelineRuler::setTimelineContext(TimelineContext* newContext)
{
    if (m_context == newContext) {
        return;
    }

    if (m_context) {
        disconnect(m_context, nullptr, this, nullptr);
    }

    m_context = newContext;

    if (m_context) {
        auto updateView = [this] () { update(); };

        connect(m_context, &TimelineContext::frameTimeChanged, this, updateView);

        connect(m_context, &TimelineContext::BPMChanged, this, updateView);
        connect(m_context, &TimelineContext::timeSigUpperChanged, this, updateView);
        connect(m_context, &TimelineContext::timeSigLowerChanged, this, updateView);
    }

    emit timelineContextChanged();
}
