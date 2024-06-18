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
    int x = 0;
    QString tickLabel;
    TickType tickType = TickType::MINORMINOR;
    QLineF line;
};

using Ticks = QVector<TickInfo>;

class TimelineRuler : public QQuickPaintedItem, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(TimelineContext * context READ timelineContext WRITE setTimelineContext NOTIFY timelineContextChanged FINAL)

    muse::Inject<muse::ui::IUiConfiguration> uiconfiguration;

signals:
    void offsetChanged();
    void zoomChanged();

public:
    explicit TimelineRuler(QQuickItem* parent = nullptr);
    ~TimelineRuler() = default;

    void setFormatter(std::unique_ptr<IRulerFormat> formatter);

    void paint(QPainter* painter) override;

    TimelineContext* timelineContext() const;
    void setTimelineContext(TimelineContext* newContext);

signals:
    void timelineContextChanged();

private:
    Ticks prepareTickData(const IntervalInfo& timeInterval, double w, double h);
    void drawLabels(QPainter* painter, const Ticks& ticks, double w, double h);
    void drawTicks(QPainter* painter, const Ticks& ticks);

    void onFrameTimeChanged();

    TimelineContext* m_context = nullptr;
    std::unique_ptr<IRulerFormat> m_formatter;
};
}
