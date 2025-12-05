/*
 * Audacity: A Digital Audio Editor
 */
#include "abstractclipview.h"

namespace au::projectscene {
AbstractClipView::AbstractClipView(QQuickItem* parent)
    : QQuickPaintedItem(parent)
{
}

void AbstractClipView::updateView()
{
    update();
}

void AbstractClipView::setClipKey(const ClipKey& newClipKey)
{
    m_clipKey = newClipKey;
    emit clipKeyChanged();
    update();
}

ClipKey AbstractClipView::clipKey() const
{
    return m_clipKey;
}

TimelineContext* AbstractClipView::timelineContext() const
{
    return m_context;
}

void AbstractClipView::setTimelineContext(TimelineContext* newContext)
{
    if (m_context == newContext) {
        return;
    }

    if (m_context) {
        disconnect(m_context, nullptr, this, nullptr);
    }

    m_context = newContext;

    if (m_context) {
        connect(m_context, &TimelineContext::frameTimeChanged, this, &AbstractClipView::updateView);
        connect(m_context, &TimelineContext::selectionStartTimeChanged, this, &AbstractClipView::updateView);
        connect(m_context, &TimelineContext::selectionEndTimeChanged, this, &AbstractClipView::updateView);
        addSpecializedConnections(*m_context);
    }

    emit timelineContextChanged();
}

bool AbstractClipView::clipSelected() const
{
    return m_clipSelected;
}

void AbstractClipView::setClipSelected(bool newClipSelected)
{
    if (m_clipSelected == newClipSelected) {
        return;
    }
    m_clipSelected = newClipSelected;
    emit clipSelectedChanged();
    update();
}

ClipTime AbstractClipView::clipTime() const
{
    return m_clipTime;
}

void AbstractClipView::setClipTime(const ClipTime& newClipTime)
{
    if (m_clipTime == newClipTime) {
        return;
    }
    m_clipTime = newClipTime;
    emit clipTimeChanged();
    update();
}

double AbstractClipView::channelHeightRatio() const
{
    return m_channelHeightRatio;
}

void AbstractClipView::setChannelHeightRatio(double channelHeightRatio)
{
    m_channelHeightRatio = channelHeightRatio;
    emit channelHeightRatioChanged();
    update();
}

int AbstractClipView::currentChannel() const
{
    return m_currentChannel.value_or(0);
}

void AbstractClipView::setCurrentChannel(int currentChannel)
{
    m_currentChannel = currentChannel;
}
}
