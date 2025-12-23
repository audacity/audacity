/*
 * Audacity: A Digital Audio Editor
 */
#include "./spectrogramview.h"
#include "framework/global/types/number.h"

namespace au::spectrogram {
SpectrogramView::SpectrogramView(QQuickItem* parent)
    : QQuickPaintedItem(parent)
{
    setFlag(QQuickItem::ItemObservesViewport, true);
}

void SpectrogramView::componentComplete()
{
    const auto project = globalContext()->currentTrackeditProject();
    IF_ASSERT_FAILED(project) {
        return;
    }
    project->trackChanged().onReceive(this, [this](const trackedit::Track& track) {
        if (track.id == m_trackId) {
            update();
        }
    });
}

void SpectrogramView::setClipId(int id)
{
    if (m_clipId == id) {
        return;
    }
    m_clipId = id;
    emit clipIdChanged();
    update();
}

void SpectrogramView::setTrackId(int id)
{
    if (m_trackId == id) {
        return;
    }
    m_trackId = id;
    emit trackIdChanged();
    update();
}

void SpectrogramView::setChannel(int channel)
{
    if (m_channel == channel) {
        return;
    }
    m_channel = channel;
    emit channelChanged();
    update();
}

void SpectrogramView::setTimelineIndentWidth(int width)
{
    if (m_timelineIndentWidth == width) {
        return;
    }
    m_timelineIndentWidth = width;
    emit timelineIndentWidthChanged();
    update();
}

void SpectrogramView::setZoom(double zoom)
{
    if (muse::is_equal(m_zoom, zoom)) {
        return;
    }
    m_zoom = zoom;
    emit zoomChanged();
    update();
}

void SpectrogramView::setFrameStartTime(double time)
{
    if (muse::is_equal(m_frameStartTime, time)) {
        return;
    }
    m_frameStartTime = time;
    emit frameStartTimeChanged();
    update();
}

void SpectrogramView::setFrameEndTime(double time)
{
    if (muse::is_equal(m_frameEndTime, time)) {
        return;
    }
    m_frameEndTime = time;
    emit frameEndTimeChanged();
    update();
}

void SpectrogramView::setSelectionStartTime(double time)
{
    if (muse::is_equal(m_selectionStartTime, time)) {
        return;
    }
    m_selectionStartTime = time;
    emit selectionStartTimeChanged();
    update();
}

void SpectrogramView::setSelectionEndTime(double time)
{
    if (muse::is_equal(m_selectionEndTime, time)) {
        return;
    }
    m_selectionEndTime = time;
    emit selectionEndTimeChanged();
    update();
}

void SpectrogramView::paint(QPainter* painter)
{
    const auto project = globalContext()->currentProject();

    const auto indentTime = m_timelineIndentWidth / m_zoom;
    const auto viewportStartTime = m_frameStartTime - indentTime;
    const auto viewportEndTime = m_frameEndTime;
    const spectrogram::SelectionInfo selectionInfo { m_selectionStartTime, m_selectionEndTime };

    const QRect visibleSubrect = clipRect().toRect();
    if (!visibleSubrect.isValid()) {
        return;
    }
    const int xBegin = std::max(visibleSubrect.left() - m_timelineIndentWidth, 0);
    const int xEnd = visibleSubrect.right() + 1;
    const spectrogram::ClipChannelInfo channelInfo { m_clipId, m_trackId, m_channel, xBegin, xEnd };
    const spectrogram::ViewInfo viewInfo {
        height(),
        viewportStartTime,
        viewportEndTime,
        m_zoom
    };

    spectrogramPainter()->paintClipChannel(*painter, channelInfo, viewInfo, selectionInfo);
}
}
