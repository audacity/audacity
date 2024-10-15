/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QQuickPaintedItem>

#include "modularity/ioc.h"
#include "iwavepainter.h"

#include "types/projectscenetypes.h"
#include "../timeline/timelinecontext.h"

class WaveClipItem;
namespace au::projectscene {
class WaveView : public QQuickPaintedItem
{
    Q_OBJECT
    Q_PROPERTY(TimelineContext * context READ timelineContext WRITE setTimelineContext NOTIFY timelineContextChanged FINAL)
    Q_PROPERTY(ClipKey clipKey READ clipKey WRITE setClipKey NOTIFY clipKeyChanged FINAL)
    Q_PROPERTY(QColor clipColor READ clipColor WRITE setClipColor NOTIFY clipColorChanged FINAL)
    Q_PROPERTY(bool clipSelected READ clipSelected WRITE setClipSelected NOTIFY clipSelectedChanged FINAL)
    Q_PROPERTY(double channelHeightRatio READ channelHeightRatio WRITE setChannelHeightRatio NOTIFY channelHeightRatioChanged FINAL)

    Q_PROPERTY(ClipTime clipTime READ clipTime WRITE setClipTime NOTIFY clipTimeChanged FINAL)

    muse::Inject<IWavePainter> wavePainter;

public:
    WaveView(QQuickItem* parent = nullptr);
    ~WaveView() override;

    TimelineContext* timelineContext() const;
    void setTimelineContext(TimelineContext* newContext);
    ClipKey clipKey() const;
    void setClipKey(const ClipKey& newClipKey);
    QColor clipColor() const;
    void setClipColor(const QColor& newClipColor);
    bool clipSelected() const;
    void setClipSelected(bool newClipSelected);
    ClipTime clipTime() const;
    void setClipTime(const ClipTime& newClipTime);
    double channelHeightRatio() const;
    void setChannelHeightRatio(double channelHeightRatio);

    Q_INVOKABLE QColor transformColor(const QColor& originalColor) const;

    void paint(QPainter* painter) override;

signals:
    void timelineContextChanged();
    void clipKeyChanged();
    void clipColorChanged();
    void clipTimeChanged();
    void clipSelectedChanged();
    void channelHeightRatioChanged();

private:

    void updateView();

    TimelineContext* m_context = nullptr;
    ClipKey m_clipKey;
    QColor m_clipColor;
    double m_clipLeft = 0;
    double m_channelHeightRatio = 0.5;
    bool m_clipSelected = false;
    ClipTime m_clipTime;
};
}
