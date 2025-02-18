/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QQuickPaintedItem>

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "iwavepainter.h"
#include "../timeline/timelinecontext.h"
#include "types/projectscenetypes.h"

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

    Q_PROPERTY(bool isNearSample READ isNearSample WRITE setIsNearSample NOTIFY isNearSampleChanged FINAL)

    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<au::projectscene::IWavePainter> wavePainter;

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
    bool isNearSample() const;
    void setIsNearSample(bool isNearSample);

    Q_INVOKABLE QColor transformColor(const QColor& originalColor) const;
    Q_INVOKABLE void setLastMousePos(const unsigned int x, const unsigned int y);
    Q_INVOKABLE void setLastClickPos(unsigned lastX, unsigned lastY, unsigned int x, const unsigned int y);

    void paint(QPainter* painter) override;

signals:
    void timelineContextChanged();
    void clipKeyChanged();
    void clipColorChanged();
    void clipTimeChanged();
    void clipSelectedChanged();
    void channelHeightRatioChanged();
    void isNearSampleChanged();

private:

    void updateView();
    IWavePainter::Params getWavePainterParams() const;

    TimelineContext* m_context = nullptr;
    ClipKey m_clipKey;
    QColor m_clipColor;
    double m_clipLeft = 0;
    double m_channelHeightRatio = 0.5;
    bool m_clipSelected = false;
    ClipTime m_clipTime;
    bool m_isNearSample = false;

    std::optional<int> m_currentChannel;
};
}
