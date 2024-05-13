#pragma once

#include <QQuickPaintedItem>

#include "modularity/ioc.h"
#include "au3wrap/iau3wavepainter.h"
#include "../../iprojectsceneconfiguration.h"

#include "types/projectscenetypes.h"
#include "timelinecontext.h"

class WaveClipItem;
namespace au::projectscene {
class WaveView : public QQuickPaintedItem
{
    Q_OBJECT
    Q_PROPERTY(TimelineContext * context READ timelineContext WRITE setTimelineContext NOTIFY timelineContextChanged FINAL)
    Q_PROPERTY(ClipKey clipKey READ clipKey WRITE setClipKey NOTIFY clipKeyChanged FINAL)

    muse::Inject<au3::IAu3WavePainter> wavePainter;
    muse::Inject<IProjectSceneConfiguration> configuration;

public:
    WaveView(QQuickItem* parent = nullptr);
    ~WaveView() override;

    ClipKey clipKey() const;
    void setClipKey(const ClipKey& newClipKey);
    TimelineContext* timelineContext() const;
    void setTimelineContext(TimelineContext* newContext);

    void paint(QPainter* painter) override;

signals:
    void clipKeyChanged();
    void timelineContextChanged();

private:
    void UpdateItemsCache(TimelineContext& trackPanelView);

    ClipKey m_clipKey;
    TimelineContext* m_context = nullptr;
};
}
