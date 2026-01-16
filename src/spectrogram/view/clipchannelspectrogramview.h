/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "internal/ispectrogrampainter.h"

#include "context/iglobalcontext.h"

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"

#include <QQuickPaintedItem>

namespace au::spectrogram {
class ClipChannelSpectrogramView : public QQuickPaintedItem, public muse::async::Asyncable
{
    Q_OBJECT
    Q_PROPERTY(int clipId READ clipId WRITE setClipId NOTIFY clipIdChanged FINAL)
    Q_PROPERTY(int trackId READ trackId WRITE setTrackId NOTIFY trackIdChanged FINAL)
    Q_PROPERTY(int channel READ channel WRITE setChannel NOTIFY channelChanged FINAL)
    Q_PROPERTY(int timelineIndentWidth READ timelineIndentWidth WRITE setTimelineIndentWidth NOTIFY timelineIndentWidthChanged FINAL)
    Q_PROPERTY(double zoom READ zoom WRITE setZoom NOTIFY zoomChanged FINAL)
    Q_PROPERTY(double frameStartTime READ frameStartTime WRITE setFrameStartTime NOTIFY frameStartTimeChanged FINAL)
    Q_PROPERTY(double frameEndTime READ frameEndTime WRITE setFrameEndTime NOTIFY frameEndTimeChanged FINAL)
    Q_PROPERTY(double selectionStartTime READ selectionStartTime WRITE setSelectionStartTime NOTIFY selectionStartTimeChanged FINAL)
    Q_PROPERTY(double selectionEndTime READ selectionEndTime WRITE setSelectionEndTime NOTIFY selectionEndTimeChanged FINAL)
    Q_PROPERTY(
        double selectionStartFrequency READ selectionStartFrequency WRITE setSelectionStartFrequency NOTIFY selectionFrequencyChanged FINAL)
    Q_PROPERTY(
        double selectionEndFrequency READ selectionEndFrequency WRITE setSelectionEndFrequency NOTIFY selectionFrequencyChanged FINAL)

    muse::Inject<ISpectrogramPainter> spectrogramPainter;
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    ClipChannelSpectrogramView(QQuickItem* parent = nullptr);
    ~ClipChannelSpectrogramView() override = default;

    int clipId() const { return m_clipId; }
    void setClipId(int id);

    int trackId() const { return m_trackId; }
    void setTrackId(int id);

    int channel() const { return m_channel; }
    void setChannel(int channel);

    int timelineIndentWidth() const { return m_timelineIndentWidth; }
    void setTimelineIndentWidth(int width);

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

    double selectionStartFrequency() const;
    void setSelectionStartFrequency(double frequency);

    double selectionEndFrequency() const;
    void setSelectionEndFrequency(double frequency);

signals:
    void clipIdChanged();
    void trackIdChanged();
    void channelChanged();
    void timelineIndentWidthChanged();
    void zoomChanged();
    void frameStartTimeChanged();
    void frameEndTimeChanged();
    void selectionStartTimeChanged();
    void selectionEndTimeChanged();
    void selectionFrequencyChanged();

private:
    void paint(QPainter* painter) override;
    void classBegin() override {}
    void componentComplete() override;

    int m_clipId = -1;
    int m_trackId = -1;
    int m_channel = 0;
    int m_timelineIndentWidth = 0;
    double m_zoom = 1.0;
    double m_frameStartTime = 0.0;
    double m_frameEndTime = 0.0;
    double m_selectionStartTime = 0.0;
    double m_selectionEndTime = 0.0;
    double m_selectionStartFrequency = SelectionInfo::UndefinedFrequency;
    double m_selectionEndFrequency = SelectionInfo::UndefinedFrequency;
};
}
