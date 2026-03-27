/*
 * Audacity: A Digital Audio Editor
 */
#include "./clipchannelspectrogramview.h"
#include "framework/global/types/number.h"

namespace au::spectrogram {
ClipChannelSpectrogramView::ClipChannelSpectrogramView(QQuickItem* parent)
    : QQuickPaintedItem(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
    setFlag(QQuickItem::ItemObservesViewport, true);
}

void ClipChannelSpectrogramView::componentComplete()
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
    spectrogramService()->trackSpectrogramConfigurationChanged().onReceive(this, [this](int trackId) {
        if (trackId == m_trackId) {
            update();
        }
    });
}

void ClipChannelSpectrogramView::setClipId(int id)
{
    if (m_clipId == id) {
        return;
    }
    m_clipId = id;
    emit clipIdChanged();
    update();
}

void ClipChannelSpectrogramView::setTrackId(int id)
{
    if (m_trackId == id) {
        return;
    }
    m_trackId = id;
    emit trackIdChanged();
    update();
}

void ClipChannelSpectrogramView::setChannel(int channel)
{
    if (m_channel == channel) {
        return;
    }
    m_channel = channel;
    emit channelChanged();
    update();
}

void ClipChannelSpectrogramView::setTimelineIndentWidth(int width)
{
    if (m_timelineIndentWidth == width) {
        return;
    }
    m_timelineIndentWidth = width;
    emit timelineIndentWidthChanged();
    update();
}

void ClipChannelSpectrogramView::setZoom(double zoom)
{
    if (muse::is_equal(m_zoom, zoom)) {
        return;
    }
    m_zoom = zoom;
    emit zoomChanged();
    update();
}

void ClipChannelSpectrogramView::setFrameStartTime(double time)
{
    if (muse::is_equal(m_frameStartTime, time)) {
        return;
    }
    m_frameStartTime = time;
    emit frameStartTimeChanged();
    update();
}

void ClipChannelSpectrogramView::setFrameEndTime(double time)
{
    if (muse::is_equal(m_frameEndTime, time)) {
        return;
    }
    m_frameEndTime = time;
    emit frameEndTimeChanged();
    update();
}

void ClipChannelSpectrogramView::setSelectionStartTime(double time)
{
    if (muse::is_equal(m_selectionStartTime, time)) {
        return;
    }
    m_selectionStartTime = time;
    emit selectionStartTimeChanged();
    update();
}

void ClipChannelSpectrogramView::setSelectionEndTime(double time)
{
    if (muse::is_equal(m_selectionEndTime, time)) {
        return;
    }
    m_selectionEndTime = time;
    emit selectionEndTimeChanged();
    update();
}

double ClipChannelSpectrogramView::selectionStartFrequency() const
{
    return m_selectionStartFrequency;
}

void ClipChannelSpectrogramView::setSelectionStartFrequency(double frequency)
{
    if (muse::is_equal(m_selectionStartFrequency, frequency)) {
        return;
    }
    m_selectionStartFrequency = frequency;
    emit selectionFrequencyChanged();
    update();
}

double ClipChannelSpectrogramView::selectionEndFrequency() const
{
    return m_selectionEndFrequency;
}

void ClipChannelSpectrogramView::setSelectionEndFrequency(double frequency)
{
    if (muse::is_equal(m_selectionEndFrequency, frequency)) {
        return;
    }
    m_selectionEndFrequency = frequency;
    emit selectionFrequencyChanged();
    update();
}

void ClipChannelSpectrogramView::paint(QPainter* painter)
{
    const auto project = globalContext()->currentProject();

    const auto indentTime = m_timelineIndentWidth / m_zoom;
    const auto viewportStartTime = m_frameStartTime - indentTime;
    const auto viewportEndTime = m_frameEndTime;

    // It can be that only one of start or end frequency is set. Only paint the selection if both are.
    const auto noSelection = m_selectionStartFrequency == SelectionInfo::UndefinedFrequency
                             || m_selectionEndFrequency == SelectionInfo::UndefinedFrequency;
    const auto startFrequency = noSelection ? SelectionInfo::UndefinedFrequency : m_selectionStartFrequency;
    const auto endFrequency = noSelection ? SelectionInfo::UndefinedFrequency : m_selectionEndFrequency;

    const SelectionInfo selectionInfo { m_selectionStartTime, m_selectionEndTime, startFrequency, endFrequency };

    const QQuickItem* const item = viewportItem();
    IF_ASSERT_FAILED(item) {
        return;
    }
    const auto boundingRect = this->boundingRect();
    const QRect rect = mapRectToItem(item, boundingRect).toRect();
    const int xBegin = std::max(-rect.left() - m_timelineIndentWidth, 0);
    const int xEnd = xBegin + boundingRect.width();
    const ClipChannelInfo channelInfo { m_clipId, m_trackId, m_channel, xBegin, xEnd };
    const ViewInfo viewInfo {
        height(),
        viewportStartTime,
        viewportEndTime,
        m_zoom
    };

    spectrogramPainter()->paintClipChannel(*painter, channelInfo, viewInfo, selectionInfo);
}
}
