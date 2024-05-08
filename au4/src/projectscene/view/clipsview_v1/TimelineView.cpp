#include "TimelineView.h"

#include "TimelineViewItem.h"
#include "TimelineContext.h"

TimelineView::TimelineView(QQuickItem* parent)
   : QQuickPaintedItem(parent)
{
   setAcceptHoverEvents(true);
}

TimelineView::~TimelineView() = default;

void TimelineView::paint(QPainter* painter)
{
   const auto trackPanelView = GetContext();
   if(trackPanelView == nullptr)
      return;

   if(mNeedsCacheUpdate)
      UpdateItemsCache(*trackPanelView);

   QRect viewRect(0, 0, width(), height());

   auto engine = qmlEngine(this);
   for(const auto& item : mItems)
      item->Paint(*engine, *painter, viewRect, *trackPanelView);
}

void TimelineView::hoverMoveEvent(QHoverEvent* )
{
}

void TimelineView::hoverEnterEvent(QHoverEvent* )
{

}

void TimelineView::hoverLeaveEvent(QHoverEvent*)
{

}

void TimelineView::mouseMoveEvent(QMouseEvent* )
{

}

void TimelineView::mousePressEvent(QMouseEvent* )
{

}

void TimelineView::mouseReleaseEvent(QMouseEvent* )
{

}

void TimelineView::keyPressEvent(QKeyEvent* )
{

}

void TimelineView::keyReleaseEvent(QKeyEvent* )
{

}

void TimelineView::setTimelineContext(TimelineContext* context)
{
   if(context == mTimelineContext)
      return;

   if(mTimelineContext != nullptr)
   {
      disconnect(mTimelineContext.get(), &TimelineContext::zoomChanged, this, nullptr);
      disconnect(mTimelineContext.get(), &TimelineContext::offsetChanged, this, nullptr);
   }

   if(context != nullptr)
   {
      mTimelineContext = context;

      connect(context, &TimelineContext::zoomChanged, this, &TimelineView::onTrackPanelZoomChanged);
      connect(context, &TimelineContext::offsetChanged, this, &TimelineView::onTrackPanelOffsetChanged);
   }
   else
      mTimelineContext = nullptr;
}

void TimelineView::geometryChange(const QRectF& newGeometry, const QRectF& oldGeometry)
{
   if(newGeometry.width() != oldGeometry.width() ||
      newGeometry.height() != oldGeometry.height())
   {
      mNeedsCacheUpdate = true;
   }
   QQuickPaintedItem::geometryChange(newGeometry, oldGeometry);
}

TimelineContext* TimelineView::GetContext() const
{
   return mTimelineContext.get();
}

void TimelineView::ResetItemsCache()
{
   mItems.clear();
   mNeedsCacheUpdate = true;

   update();
}

void TimelineView::ItemRemoved(const TimelineViewItem* item)
{
   auto it = std::find(mItems.begin(), mItems.end(), item);
   if(it != mItems.end())
      mItems.erase(it);
}

void TimelineView::ItemAdded(TimelineViewItem* item)
{
   mItems.push_back(item);
}

TimelineContext* TimelineView::FindContext(QQuickItem* item)
{;
   while(item != nullptr)
   {
      if(const auto timelineContext = qobject_cast<TimelineContext*>(item))
         return timelineContext;
      item = item->parentItem();
   }
   return nullptr;
}

void TimelineView::processHoverEvent(QHoverEvent* )
{

}

void TimelineView::onTrackPanelOffsetChanged()
{
   mNeedsCacheUpdate = true;
   update();
}

void TimelineView::onTrackPanelZoomChanged()
{
   mNeedsCacheUpdate = true;
   update();
}
