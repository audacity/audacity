#include "TimelineView.h"

#include "TimelineViewItem.h"
#include "TimelineViewUIHandle.h"
#include "TimelineContext.h"
#include "handles/TimeSelectionHandle.h"

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

void TimelineView::hoverMoveEvent(QHoverEvent* event)
{
   processHoverEvent(event);
}

void TimelineView::hoverEnterEvent(QHoverEvent* event)
{
   mFocusedItem = nullptr;

   UnsetUIHandle();

   processHoverEvent(event);
}

void TimelineView::hoverLeaveEvent(QHoverEvent*)
{
   mFocusedItem = nullptr;
   UnsetUIHandle();
}

void TimelineView::mouseMoveEvent(QMouseEvent* event)
{
   if(mUIHandle)
      mUIHandle->OnMouseMove(*this);
}

void TimelineView::mousePressEvent(QMouseEvent* event)
{
   if(mUIHandle)
   {
      mActiveItem = mActiveItem;
      mUIHandle->OnMousePress(*this);
   }
}

void TimelineView::mouseReleaseEvent(QMouseEvent* event)
{
   if(mUIHandle)
   {
      mUIHandle->OnMouseRelease(*this);
      UnsetUIHandle();
   }
}

void TimelineView::keyPressEvent(QKeyEvent* event)
{
   if(mActiveItem != nullptr)
   {
      
   }
}

void TimelineView::keyReleaseEvent(QKeyEvent* event)
{
   if(mActiveItem != nullptr)
   {
      
   }
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
   UnsetUIHandle();
   
   mFocusedItem = nullptr;
   mActiveItem = nullptr;
   mItems.clear();
   mNeedsCacheUpdate = true;

   update();
}

void TimelineView::ItemRemoved(const TimelineViewItem* item)
{
   if(item == mFocusedItem)
      mFocusedItem = nullptr;
   if(item == mActiveItem)
      mActiveItem = nullptr;

   auto it = std::find(mItems.begin(), mItems.end(), item);
   if(it != mItems.end())
      mItems.erase(it);
}

void TimelineView::ItemAdded(TimelineViewItem* item)
{
   mItems.push_back(item);
}

void TimelineView::UnsetUIHandle()
{
   mUIHandle.reset();
   unsetCursor();
   setKeepMouseGrab(false);
   setKeepTouchGrab(false);
   setAcceptedMouseButtons(Qt::NoButton);
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

void TimelineView::processHoverEvent(QHoverEvent* event)
{
   UnsetUIHandle();

   for(const auto item : mItems)
   {
      if(auto handle = item->HitTest(event->pos()))
      {
         mFocusedItem = item;
         mUIHandle = std::move(handle);
         break;
      }
   }

   if(!mUIHandle)
      mUIHandle = std::make_unique<TimeSelectionHandle>();
   
   mUIHandle->OnMouseEnter(*this);
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
