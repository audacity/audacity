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

    //! NOTE In a static position, the slip start time corresponds,
    //! but it can change while dragging the clip, with the not changing start time
    //! (until the end of dragging)
    Q_PROPERTY(double clipLeft READ clipLeft WRITE setClipLeft NOTIFY clipLeftChanged FINAL)

    muse::Inject<au3::IAu3WavePainter> wavePainter;
    muse::Inject<IProjectSceneConfiguration> configuration;

public:
    WaveView(QQuickItem* parent = nullptr);
    ~WaveView() override;

    ClipKey clipKey() const;
    void setClipKey(const ClipKey& newClipKey);
    TimelineContext* timelineContext() const;
    void setTimelineContext(TimelineContext* newContext);
    double clipLeft() const;
    void setClipLeft(double newClipLeft);

    void paint(QPainter* painter) override;

signals:
    void clipKeyChanged();
    void timelineContextChanged();
    void clipLeftChanged();

private:

    void onFrameTimeChanged();

    ClipKey m_clipKey;
    TimelineContext* m_context = nullptr;
    double m_clipLeft = 0;
};
}
