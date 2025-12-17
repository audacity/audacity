/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "ispectrogrampainter.h"

#include "context/iglobalcontext.h"

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"

#include <QQuickPaintedItem>

namespace au::spectrogram {
class SpectrogramView : public QQuickPaintedItem, public muse::async::Asyncable
{
    Q_OBJECT
    Q_PROPERTY(int clipId READ clipId WRITE setClipId NOTIFY clipIdChanged FINAL)
    Q_PROPERTY(int trackId READ trackId WRITE setTrackId NOTIFY trackIdChanged FINAL)
    Q_PROPERTY(int timelineIndentWidth READ timelineIndentWidth WRITE setTimelineIndentWidth NOTIFY timelineIndentWidthChanged FINAL)
    Q_PROPERTY(double channelHeightRatio READ channelHeightRatio WRITE setChannelHeightRatio NOTIFY channelHeightRatioChanged FINAL)
    Q_PROPERTY(double zoom READ zoom WRITE setZoom NOTIFY zoomChanged FINAL)
    Q_PROPERTY(double frameStartTime READ frameStartTime WRITE setFrameStartTime NOTIFY frameStartTimeChanged FINAL)
    Q_PROPERTY(double frameEndTime READ frameEndTime WRITE setFrameEndTime NOTIFY frameEndTimeChanged FINAL)
    Q_PROPERTY(double selectionStartTime READ selectionStartTime WRITE setSelectionStartTime NOTIFY selectionStartTimeChanged FINAL)
    Q_PROPERTY(double selectionEndTime READ selectionEndTime WRITE setSelectionEndTime NOTIFY selectionEndTimeChanged FINAL)

    muse::Inject<ISpectrogramPainter> spectrogramPainter;
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    SpectrogramView(QQuickItem* parent = nullptr);
    ~SpectrogramView() override = default;

    int clipId() const { return m_clipId; }
    void setClipId(int id);

    int trackId() const { return m_trackId; }
    void setTrackId(int id);

    int timelineIndentWidth() const { return m_timelineIndentWidth; }
    void setTimelineIndentWidth(int width);

    double channelHeightRatio() const { return m_channelHeightRatio; }
    void setChannelHeightRatio(double ratio);

    double zoom() const { return m_zoom; }
    void setZoom(double zoom);

    double frameStartTime() const { return m_frameStartTime; }
    void setFrameStartTime(double time);

    double frameEndTime() const { return m_frameEndTime; }
    void setFrameEndTime(double time);

    double selectionStartTime() const { return m_selectionStartTime; }
    void setSelectionStartTime(double time);

    double selectionEndTime() const { return m_selectionEndTime; }
    void setSelectionEndTime(double time);

signals:
    void clipIdChanged();
    void trackIdChanged();
    void timelineIndentWidthChanged();
    void channelHeightRatioChanged();
    void zoomChanged();
    void frameStartTimeChanged();
    void frameEndTimeChanged();
    void selectionStartTimeChanged();
    void selectionEndTimeChanged();

private:
    void paint(QPainter* painter) override;
    void classBegin() override {}
    void componentComplete() override;

    int m_clipId = -1;
    int m_trackId = -1;
    int m_timelineIndentWidth = 0;
    double m_channelHeightRatio = 0.5;
    double m_zoom = 1.0;
    double m_frameStartTime = 0.0;
    double m_frameEndTime = 0.0;
    double m_selectionStartTime = 0.0;
    double m_selectionEndTime = 0.0;
};
}
