#include "WaveView.h"

#include <QPainter>

#include "WaveClip.h"
#include "TimelineContext.h"
#include "ViewInfo.h"
#include "items/WaveClipItem.h"

#include "log.h"

using namespace au::projectscene;

WaveView::WaveView(QQuickItem* parent)
    : QQuickPaintedItem(parent)
{
    setAcceptHoverEvents(true);
}

WaveView::~WaveView()
{
    delete m_item;
}

void WaveView::setClipKey(const ClipKey& newClipKey)
{
    m_clipKey = newClipKey;
    emit clipKeyChanged();

    delete m_item;
    m_item = nullptr;
    m_needsCacheUpdate = true;

    update();
}

void WaveView::paint(QPainter* painter)
{
    if (m_needsCacheUpdate) {
        UpdateItemsCache(*m_context);
        m_needsCacheUpdate = false;
    }

    QRect viewRect(0, 0, width(), height());

    m_item->Paint(*painter, viewRect, *m_context);
}

void WaveView::geometryChange(const QRectF& newGeometry, const QRectF& oldGeometry)
{
    if (newGeometry.width() != oldGeometry.width()
        || newGeometry.height() != oldGeometry.height()) {
        m_needsCacheUpdate = true;
    }
    QQuickPaintedItem::geometryChange(newGeometry, oldGeometry);
}

void WaveView::UpdateItemsCache(TimelineContext& trackPanelView)
{
    assert(m_clipKey.au3WaveClipPtr && m_clipKey.au3WaveTrackPtr);

    WaveTrack* track = reinterpret_cast<WaveTrack* >(m_clipKey.au3WaveTrackPtr);
    WaveClip* clip = reinterpret_cast<WaveClip* >(m_clipKey.au3WaveClipPtr);

    const auto viewRect = QRect(
        0,
        0,
        static_cast<int>(width()),
        static_cast<int>(height()));

    const ViewInfo viewInfo(trackPanelView.offset(),  trackPanelView.zoom());

    //If clip is "too small" draw a placeholder instead of
    //attempting to fit the contents into a few pixels
    if (!WaveClipItem::ClipDetailsVisible(*clip, viewInfo, viewRect)) {
        return;
    }

    const auto left = 0;//(interval->GetPlayStartTime() - trackPanelView.offset()) * trackPanelView.zoom();
    const auto width = (clip->GetPlayEndTime() - clip->GetPlayStartTime()) * trackPanelView.zoom();

    if (left >= viewRect.right() || left + width <= viewRect.left()) {
        return;
    }

    m_item = new WaveClipItem(*track);
    m_item->SetClip(clip);
}

ClipKey WaveView::clipKey() const
{
    return m_clipKey;
}

TimelineContext* WaveView::timelineContext() const
{
    return m_context;
}

void WaveView::setTimelineContext(TimelineContext* newContext)
{
    if (m_context == newContext) {
        return;
    }

    //! TODO Subscribe on context props changes
    m_context = newContext;
    emit timelineContextChanged();
}
