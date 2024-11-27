#pragma once

#include <QtGui/QColor>
#include <QtGui/QFont>
#include <QtQuick/QQuickPaintedItem>

#include "modularity/ioc.h"
#include "ui/iuiconfiguration.h"
#include "async/asyncable.h"

#include "timeformat.h"
#include "beatsmeasuresformat.h"
#include "timelinecontext.h"

class QPainter;

namespace au::projectscene {
struct TickInfo {
    double x = 0.0;
    QString tickLabel;
    TickType tickType = TickType::MINORMINOR;
    QLineF line;
    double timeValue;
};

using Ticks = QVector<TickInfo>;

class TimelineRuler : public QQuickPaintedItem, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(TimelineContext * context READ timelineContext WRITE setTimelineContext NOTIFY timelineContextChanged FINAL)

    muse::Inject<muse::ui::IUiConfiguration> uiconfiguration;
    muse::Inject<IProjectSceneConfiguration> configuration;

signals:
    void offsetChanged();
    void zoomChanged();

public:
    explicit TimelineRuler(QQuickItem* parent = nullptr);
    ~TimelineRuler() = default;

    Q_INVOKABLE bool isMajorSection(int y) const;

    IntervalInfo intervalInfo();

    void setFormatter(const TimelineRulerMode mode);

    void paint(QPainter* painter) override;

    TimelineContext* timelineContext() const;
    void setTimelineContext(TimelineContext* newContext);

signals:
    void timelineContextChanged();
    void ticksChanged(Ticks ticks);
    void formatterChanged();

private:
    Ticks prepareTickData(const IntervalInfo& timeInterval, double w, double h);
    TickType determineTickType(double value, const IntervalInfo& timeInterval);
    void drawLabels(QPainter* painter, const Ticks& ticks, double w, double h);
    void drawTicks(QPainter* painter, const Ticks& ticks);

    TimelineContext* m_context = nullptr;
    std::unique_ptr<IRulerFormat> m_formatter;
};
}
