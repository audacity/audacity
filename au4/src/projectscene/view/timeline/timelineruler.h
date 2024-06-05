#pragma once

#include <QtGui/QColor>
#include <QtGui/QFont>
#include <QtQuick/QQuickPaintedItem>

#include "modularity/ioc.h"
#include "ui/iuiconfiguration.h"
#include "async/asyncable.h"

#include "timeformat.h"
#include "timelinecontext.h"

struct TickInfo {
    int x = 0;
    QString tickLabel;
    TickType tickType = TickType::MINORMINOR;
    QLineF line;
};

using Ticks = QVector<TickInfo>;

class QPainter;
class TimelineRuler : public QQuickPaintedItem, public muse::async::Asyncable
{
    Q_OBJECT

    muse::Inject<muse::ui::IUiConfiguration> uiconfiguration;

    Q_PROPERTY(TimelineContext * context READ timelineContext WRITE setTimelineContext NOTIFY timelineContextChanged FINAL)

signals:
    void offsetChanged();
    void zoomChanged();

public:
    explicit TimelineRuler(QQuickItem* parent = nullptr);
    ~TimelineRuler() = default;

    void paint(QPainter* painter) override;

    TimelineContext* timelineContext() const;
    void setTimelineContext(TimelineContext* newContext);

signals:
    void timelineContextChanged();

private:
    void prepareTickData(Ticks& ticks, const TimeIntervalInfo& timeInterval, double w, double h);
    void drawLabels(QPainter* painter, const Ticks& ticks, double w, double h);
    void drawTicks(QPainter* painter, const Ticks& ticks);

    void onFrameTimeChanged();

    TimelineContext* m_context = nullptr;
};
