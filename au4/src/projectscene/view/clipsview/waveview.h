#pragma once

#include <QQuickPaintedItem>

#include "modularity/ioc.h"
#include "au3wrap/iau3wavepainter.h"
#include "../../iprojectsceneconfiguration.h"

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

    //! NOTE In a static position, the slip start time corresponds,
    //! but it can change while dragging the clip, with the not changing start time
    //! (until the end of dragging)
    Q_PROPERTY(double clipLeft READ clipLeft WRITE setClipLeft NOTIFY clipLeftChanged FINAL)

    muse::Inject<au3::IAu3WavePainter> wavePainter;
    muse::Inject<IProjectSceneConfiguration> configuration;

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
    double clipLeft() const;
    void setClipLeft(double newClipLeft);

    void paint(QPainter* painter) override;

signals:
    void timelineContextChanged();
    void clipKeyChanged();
    void clipColorChanged();
    void clipLeftChanged();
    void clipSelectedChanged();

private:

    void onFrameTimeChanged();

    TimelineContext* m_context = nullptr;
    ClipKey m_clipKey;
    QColor m_clipColor;
    double m_clipLeft = 0;
    bool m_clipSelected = false;
};
}
