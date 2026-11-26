/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <vector>

#include <QColor>
#include <QQuickItem>
#include <QRectF>

#include "async/asyncable.h"

#include "view/timeline/timelineruler.h"

class QSGGeometryNode;

namespace au::projectscene {
class GridLines : public QQuickItem, public muse::async::Asyncable, public muse::Contextable
{
    Q_OBJECT

    Q_PROPERTY(TimelineRuler * timelineRuler READ timelineRuler WRITE setTimelineRuler NOTIFY timelineRulerChanged FINAL)

    muse::GlobalInject<muse::ui::IUiConfiguration> uiconfiguration;
    muse::ContextInject<IProjectSceneUiState> projectSceneUiState { this };

public:
    explicit GridLines(QQuickItem* parent = nullptr);
    ~GridLines() = default;

    TimelineRuler* timelineRuler() const;
    void setTimelineRuler(TimelineRuler* newTimelineRuler);

signals:
    void timelineRulerChanged();

private:
    using Rects = std::vector<QRectF>;

    QSGNode* updatePaintNode(QSGNode* oldNode, UpdatePaintNodeData*) override;
    void geometryChange(const QRectF& newGeometry, const QRectF& oldGeometry) override;

    void collectGridLines(const Ticks& ticks, Rects& minorLines, Rects& majorLines) const;
    void collectZebraHighlighting(const Ticks& ticks, Rects& highlights) const;
    static void fillNode(QSGGeometryNode* node, const Rects& rects, const QColor& color);

    TimelineRuler* m_timelineRuler = nullptr;
};
}
