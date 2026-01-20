/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QQuickPaintedItem>

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "trackedit/iprojecthistory.h"
#include "global/async/asyncable.h"

#include "iwavepainter.h"
#include "../timeline/timelinecontext.h"
#include "types/projectscenetypes.h"

class WaveClipItem;
namespace au::projectscene {
class WaveView : public QQuickPaintedItem, public muse::async::Asyncable, public muse::Injectable
{
    Q_OBJECT
    Q_PROPERTY(TimelineContext * context READ timelineContext WRITE setTimelineContext NOTIFY timelineContextChanged FINAL)
    Q_PROPERTY(ClipKey clipKey READ clipKey WRITE setClipKey NOTIFY clipKeyChanged FINAL)
    Q_PROPERTY(QColor clipColor READ clipColor WRITE setClipColor NOTIFY clipColorChanged FINAL)
    Q_PROPERTY(bool clipSelected READ clipSelected WRITE setClipSelected NOTIFY clipSelectedChanged FINAL)
    Q_PROPERTY(double channelHeightRatio READ channelHeightRatio WRITE setChannelHeightRatio NOTIFY channelHeightRatioChanged FINAL)

    Q_PROPERTY(ClipTime clipTime READ clipTime WRITE setClipTime NOTIFY clipTimeChanged FINAL)

    Q_PROPERTY(bool isNearSample READ isNearSample WRITE setIsNearSample NOTIFY isNearSampleChanged FINAL)
    Q_PROPERTY(bool isStemPlot READ isStemPlot WRITE setIsStemPlot NOTIFY isStemPlotChanged FINAL)
    Q_PROPERTY(int currentChannel READ currentChannel WRITE setCurrentChannel FINAL)
    Q_PROPERTY(bool isIsolationMode READ isIsolationMode WRITE setIsIsolationMode NOTIFY isIsolationModeChanged FINAL)
    Q_PROPERTY(bool multiSampleEdit READ multiSampleEdit WRITE setMultiSampleEdit NOTIFY multiSampleEditChanged FINAL)
    Q_PROPERTY(bool isBrush READ isBrush WRITE setIsBrush NOTIFY isBrushChanged FINAL)
    Q_PROPERTY(bool isLinear READ isLinear WRITE setIsLinear FINAL)
    Q_PROPERTY(double dbRange READ dbRange WRITE setDbRange FINAL)
    Q_PROPERTY(QVariant displayBounds READ displayBounds WRITE setDisplayBounds FINAL)

    muse::GlobalInject<IProjectSceneConfiguration> configuration;

    muse::Inject<au::context::IGlobalContext> globalContext{ this };
    muse::Inject<au::projectscene::IWavePainter> wavePainter{ this };
    muse::Inject<au::trackedit::IProjectHistory> projectHistory{ this };

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
    bool isStemPlot() const;
    void setIsStemPlot(bool isStemPlot);
    int currentChannel() const;
    void setCurrentChannel(int currentChannel);
    bool isIsolationMode() const;
    void setIsIsolationMode(bool isIsolationMode);
    bool multiSampleEdit() const;
    void setMultiSampleEdit(bool multiSampleEdit);
    bool isBrush() const;
    void setIsBrush(bool isBrush);
    bool isLinear() const;
    void setIsLinear(bool isLinear);
    double dbRange() const;
    void setDbRange(double dbRange);
    QVariant displayBounds() const;
    void setDisplayBounds(const QVariant& displayBounds);

    Q_INVOKABLE QColor transformColor(const QColor& originalColor) const;
    Q_INVOKABLE void setLastMousePos(const unsigned int x, const unsigned int y);
    Q_INVOKABLE void setLastClickPos(unsigned lastX, unsigned lastY, unsigned int x, const unsigned int y);
    Q_INVOKABLE void smoothLastClickPos(unsigned int x, const unsigned int y);
    Q_INVOKABLE void setIsolatedPoint(unsigned int x, unsigned int y);

    void paint(QPainter* painter) override;

signals:
    void timelineContextChanged();
    void clipKeyChanged();
    void clipColorChanged();
    void clipTimeChanged();
    void clipSelectedChanged();
    void channelHeightRatioChanged();
    void isNearSampleChanged();
    void isStemPlotChanged();
    void isIsolationModeChanged();
    void multiSampleEditChanged();
    void isBrushChanged();

private:

    void updateView();
    void onWaveZoomChanged();
    IWavePainter::Params getWavePainterParams() const;
    void applyColorfulStyle(IWavePainter::Params& params, const QColor& clipColor, bool selected) const;
    void applyClassicStyle(IWavePainter::Params& params, bool selected) const;
    void pushProjectHistorySampleEdit();

    context::IPlaybackStatePtr playbackState() const;

    TimelineContext* m_context = nullptr;
    ClipKey m_clipKey;
    QColor m_clipColor;
    double m_clipLeft = 0;
    double m_channelHeightRatio = 0.5;
    bool m_clipSelected = false;
    ClipTime m_clipTime;
    bool m_isNearSample = false;
    bool m_isStemPlot = false;
    bool m_isIsolationMode = false;
    bool m_multiSampleEdit = false;
    bool m_isBrush = false;
    bool m_isLinear = false;
    double m_dbRange = -60.0;
    std::pair<float, float> m_displayBounds = { -1.0f, 1.0f };

    std::optional<int> m_currentChannel;
    std::optional<QPoint> m_lastClickedPoint;
};
}
