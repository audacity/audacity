#pragma once

#include <QObject>

#include "global/async/asyncable.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "actions/iactionsdispatcher.h"

#include "../clipsview/timelinecontext.h"

namespace au::projectscene {
class PlayCursorModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(TimelineContext * context READ timelineContext WRITE setTimelineContext NOTIFY timelineContextChanged FINAL)

    Q_PROPERTY(double positionX READ positionX NOTIFY positionXChanged FINAL)

    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher;

public:
    PlayCursorModel(QObject* parent = nullptr);

    TimelineContext* timelineContext() const;
    void setTimelineContext(TimelineContext* newContext);
    double positionX() const;

    Q_INVOKABLE void init();
    Q_INVOKABLE void seekToX(double x);

signals:
    void timelineContextChanged();
    void positionXChanged();

private:

    context::IPlaybackStatePtr playbackState() const;

    void updatePositionX(audio::secs_t secs);

    TimelineContext* m_context = nullptr;
    double m_positionX = 0.0;
};
}
