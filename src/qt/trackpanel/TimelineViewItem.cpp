#include "TimelineViewItem.h"
#include "TimelineViewUIHandle.h"

TimelineViewItem::~TimelineViewItem() = default;

void TimelineViewItem::Paint(QQmlEngine& engine, QPainter& painter, const QRect& viewRect, const TimelineContext& trackPanelView)
{
}

std::unique_ptr<TimelineViewUIHandle> TimelineViewItem::HitTest(const QPoint& at) const
{
   return {};
}
