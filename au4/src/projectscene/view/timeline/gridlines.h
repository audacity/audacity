/*
* Audacity: A Digital Audio Editor
*/

#include "qquickpainteditem.h"
#include "view/timeline/timelineruler.h"

class QPainter;

namespace au::projectscene {
class GridLines : public QQuickPaintedItem
{
    Q_OBJECT

    Q_PROPERTY(TimelineRuler * timelineRuler READ timelineRuler WRITE setTimelineRuler NOTIFY timelineRulerChanged FINAL)

    muse::Inject<muse::ui::IUiConfiguration> uiconfiguration;
    muse::Inject<IProjectSceneConfiguration> configuration;

public:
    explicit GridLines(QQuickItem* parent = nullptr);
    ~GridLines() = default;

    void paint(QPainter* painter) override;

    TimelineRuler* timelineRuler() const;
    void setTimelineRuler(TimelineRuler* newTimelineRuler);

signals:
    void timelineRulerChanged();

private:
    void drawGridLines(QPainter* painter);
    void drawZebraHighlighting(QPainter* painter);
    void highlightHalfSection(QPainter* painter, qreal h, qreal w, const QColor& color, bool firstHalf, double nextTick,
                              const IntervalInfo& intervalInfo);

    TimelineRuler* m_timelineRuler = nullptr;
    Ticks m_ticks = {};
};
}
