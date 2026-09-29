/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QQuickPaintedItem>

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "trackedit/iprojecthistory.h"
#include "iprojectsceneconfiguration.h"
#include "global/async/asyncable.h"

#include "../timeline/timelinecontext.h"

namespace au::projectscene {
//! Debug overlay showing the sample blocks of every clip of a track, including
//! the parts of blocks that lie in trimmed (hidden) regions.
//! Toggled with Diagnostics > Project > Show sample blocks.
class SampleBlocksOverlay : public QQuickPaintedItem, public muse::async::Asyncable, public muse::Contextable
{
    Q_OBJECT

    Q_PROPERTY(bool blocksVisible READ blocksVisible NOTIFY blocksVisibleChanged FINAL)
    Q_PROPERTY(QVariant trackId READ trackId WRITE setTrackId NOTIFY trackIdChanged FINAL)
    Q_PROPERTY(TimelineContext * context READ timelineContext WRITE setTimelineContext NOTIFY timelineContextChanged FINAL)
    Q_PROPERTY(double channelHeightRatio READ channelHeightRatio WRITE setChannelHeightRatio NOTIFY channelHeightRatioChanged FINAL)

    muse::ContextInject<au::context::IGlobalContext> globalContext{ this };
    muse::ContextInject<trackedit::IProjectHistory> projectHistory{ this };
    muse::GlobalInject<IProjectSceneConfiguration> configuration;

public:
    explicit SampleBlocksOverlay(QQuickItem* parent = nullptr);

    void paint(QPainter* painter) override;
    void componentComplete() override;

    bool blocksVisible() const;

    QVariant trackId() const;
    void setTrackId(const QVariant& trackId);

    TimelineContext* timelineContext() const;
    void setTimelineContext(TimelineContext* context);

    double channelHeightRatio() const;
    void setChannelHeightRatio(double ratio);

signals:
    void blocksVisibleChanged();
    void trackIdChanged();
    void timelineContextChanged();
    void channelHeightRatioChanged();

private:
    void subscribeToProject();

    trackedit::TrackId m_trackId = -1;
    TimelineContext* m_context = nullptr;
    double m_channelHeightRatio = 0.5;
};
}
