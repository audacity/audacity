#pragma once

#include <QQuickPaintedItem>

#include "clipkey.h"
#include "TimelineContext.h"

class WaveClipItem;
namespace au::projectscene {
class WaveView : public QQuickPaintedItem
{
    Q_OBJECT
    Q_PROPERTY(TimelineContext * context READ timelineContext WRITE setTimelineContext NOTIFY timelineContextChanged FINAL)
    Q_PROPERTY(ClipKey clipKey READ clipKey WRITE setClipKey NOTIFY clipKeyChanged FINAL)

public:
    WaveView(QQuickItem* parent = nullptr);
    ~WaveView() override;

    ClipKey clipKey() const;
    void setClipKey(const ClipKey& newClipKey);
    TimelineContext* timelineContext() const;
    void setTimelineContext(TimelineContext* newContext);

    void paint(QPainter* painter) override;
    void geometryChange(const QRectF& newGeometry, const QRectF& oldGeometry) override;

signals:
    void clipKeyChanged();
    void timelineContextChanged();

private:
    void UpdateItemsCache(TimelineContext& trackPanelView);

    ClipKey m_clipKey;
    WaveClipItem* m_item = nullptr;
    TimelineContext* m_context = nullptr;
    bool m_needsCacheUpdate = false;
};
}
