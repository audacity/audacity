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
constexpr int LABEL_INTERVAL = 5;
}

using namespace au::projectscene;

TimelineRuler::TimelineRuler(QQuickItem* parent)
    : QQuickPaintedItem(parent)
{
    uiconfiguration()->currentThemeChanged().onNotify(this, [this]() { update(); });
}

void TimelineRuler::paint(QPainter* painter)
{
    const qreal w = width();
    const qreal h = height();

    // determine current time interval and prepare ticks
    // IntervalInfo interval = TimeFormat::intervalInfo(m_context);
    IntervalInfo interval = BeatsMeasuresFormat::intervalInfo(m_context);
    Ticks ticks = prepareTickData(interval, w, h);

    // begin painting
    QPen pen = painter->pen();
    pen.setWidth(1);
    pen.setColor(uiconfiguration()->currentTheme().values.value(muse::ui::STROKE_COLOR).toString());
    painter->setPen(pen);

    // vertical line (ruler border)
    painter->drawLine(QLineF(0, 0, 0, h));

    // horizontal line in the middle
    painter->drawLine(QLineF(0, h / 2, w, h / 2));

    drawLabels(painter, ticks, w, h);
    drawTicks(painter, ticks);
}

Ticks TimelineRuler::prepareTickData(const IntervalInfo& timeInterval, double w, double h)
{
    Ticks ticks;
    double value = m_context->frameStartTime();
    double x = 0.0;

    // find value and position of the first tick
    double remainder = fmod(value, timeInterval.minorMinor);
    if (remainder != 0) {
        x = (timeInterval.minorMinor - remainder) * m_context->zoom();
        value += (timeInterval.minorMinor - remainder);
    }
    // determine which tick in a row is this
    int tickNumber = static_cast<int>(value / timeInterval.minorMinor);

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
        TickType tickType;
        if (tickNumber % static_cast<int>(timeInterval.major / timeInterval.minorMinor) == 0) {
            tickType = TickType::MAJOR;
        } else if (tickNumber % static_cast<int>(timeInterval.minor / timeInterval.minorMinor) == 0) {
            tickType = TickType::MINOR;
        } else {
            tickType = TickType::MINORMINOR;
        }

        // QString tickLabel = TimeFormat::label(value, timeInterval, tickType);
        QString tickLabel = BeatsMeasuresFormat::label(value, timeInterval, tickType, m_context);
        int labelsCount = 0;
        //! AU4 TODO: when having very small distance between ticks, ticks look not even
        if (tickType == TickType::MAJOR || tickType == TickType::MINOR) {
            // add tick with label
            ticks.append(TickInfo { static_cast<int>(std::round(x) + (labelsCount % LABEL_INTERVAL == 0 ? LABEL_OFFSET : 0)),
                                    tickLabel,
                                    tickType,
                                    QLineF(std::round(x), h - 2, std::round(x), h - 1 - tickHeight(tickType)) });
            labelsCount++;
        } else {
            // add tick without label
            ticks.append(TickInfo { -1,
                                    QString(),
                                    tickType,
                                    QLineF(std::round(x), h - 2, std::round(x), h - 1 - tickHeight(tickType)) });
        }

        x += m_context->zoom() * timeInterval.minorMinor;
        value += timeInterval.minorMinor;
        tickNumber++;
    }

    return ticks;
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
        if (ticks[i].x == -1) {
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
        connect(m_context, &TimelineContext::frameTimeChanged, this, &TimelineRuler::onFrameTimeChanged);
    }

    emit timelineContextChanged();
}

void TimelineRuler::onFrameTimeChanged()
{
    update();
}
