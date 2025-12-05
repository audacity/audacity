/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../timeline/timelinecontext.h"
#include "types/projectscenetypes.h"

#include <QQuickPaintedItem>

namespace au::projectscene {
class AbstractClipView : public QQuickPaintedItem
{
    Q_OBJECT
    Q_PROPERTY(TimelineContext * context READ timelineContext WRITE setTimelineContext NOTIFY timelineContextChanged FINAL)
    Q_PROPERTY(ClipKey clipKey READ clipKey WRITE setClipKey NOTIFY clipKeyChanged FINAL)
    Q_PROPERTY(bool clipSelected READ clipSelected WRITE setClipSelected NOTIFY clipSelectedChanged FINAL)
    Q_PROPERTY(double channelHeightRatio READ channelHeightRatio WRITE setChannelHeightRatio NOTIFY channelHeightRatioChanged FINAL)
    Q_PROPERTY(ClipTime clipTime READ clipTime WRITE setClipTime NOTIFY clipTimeChanged FINAL)
    Q_PROPERTY(int currentChannel READ currentChannel WRITE setCurrentChannel FINAL)

public:
    AbstractClipView(QQuickItem* parent = nullptr);

    TimelineContext* timelineContext() const;
    void setTimelineContext(TimelineContext* newContext);

    ClipKey clipKey() const;
    void setClipKey(const ClipKey& newClipKey);

    bool clipSelected() const;
    void setClipSelected(bool newClipSelected);

    ClipTime clipTime() const;
    void setClipTime(const ClipTime& newClipTime);

    double channelHeightRatio() const;
    void setChannelHeightRatio(double channelHeightRatio);

    int currentChannel() const;
    void setCurrentChannel(int currentChannel);

signals:
    void timelineContextChanged();
    void clipKeyChanged();
    void clipTimeChanged();
    void clipSelectedChanged();
    void channelHeightRatioChanged();

protected:
    TimelineContext* m_context = nullptr;
    ClipKey m_clipKey;
    double m_channelHeightRatio = 0.5;
    bool m_clipSelected = false;
    ClipTime m_clipTime;
    std::optional<int> m_currentChannel;

private:
    virtual void addSpecializedConnections(TimelineContext&) {}
    void updateView();
};
}
