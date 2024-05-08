#pragma once

#include <QQuickPaintedItem>

class TimelineViewUIHandle;
class TimelineContext;
class TimelineViewItem;

class TimelineView : public QQuickPaintedItem
{
    Q_OBJECT
    QML_ELEMENT

    Q_PROPERTY(TimelineContext* context READ GetContext WRITE setTimelineContext FINAL)

public:

    TimelineView(QQuickItem* parent = nullptr);
    ~TimelineView() override;

    void paint(QPainter* painter) override;

    void hoverMoveEvent(QHoverEvent* event) override;
    void hoverEnterEvent(QHoverEvent* event) override;
    void hoverLeaveEvent(QHoverEvent* event) override;

    void mouseMoveEvent(QMouseEvent* event) override;
    void mousePressEvent(QMouseEvent* event) override;
    void mouseReleaseEvent(QMouseEvent* event) override;

    void keyPressEvent(QKeyEvent* event) override;
    void keyReleaseEvent(QKeyEvent* event) override;

    void setTimelineContext(TimelineContext* context);

protected:

    void geometryChange(const QRectF& newGeometry, const QRectF& oldGeometry) override;

    TimelineContext* GetContext() const;

    virtual void UpdateItemsCache(TimelineContext& trackPanelView) = 0;

    void ResetItemsCache();
    void ItemRemoved(const TimelineViewItem* item);
    void ItemAdded(TimelineViewItem* item);

private:

    static TimelineContext* FindContext(QQuickItem* item);

    void processHoverEvent(QHoverEvent* event);

    void onTrackPanelOffsetChanged();
    void onTrackPanelZoomChanged();

    bool mNeedsCacheUpdate { true };

    std::vector<TimelineViewItem*> mItems;
    QPointer<TimelineContext> mTimelineContext;
};
