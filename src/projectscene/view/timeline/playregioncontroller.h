/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <QObject>

#include "modularity/ioc.h"
#include "playback/iplayback.h"

#include "timelinecontext.h"

namespace au::projectscene {
class PlayRegionController : public QObject
{
    Q_OBJECT

    Q_PROPERTY(TimelineContext * context READ context WRITE setContext NOTIFY contextChanged FINAL)

    muse::Inject<playback::IPlayback> playback;

    enum class UserInputAction {
        None,
        CreateRegion,
        ResizeStart,
        ResizeEnd,
        Drag
    };

public:
    PlayRegionController(QObject* parent = nullptr);

    Q_INVOKABLE void mouseDown(double pos);
    Q_INVOKABLE void mouseUp(double pos);
    Q_INVOKABLE void mouseMove(double pos);

    Q_INVOKABLE void beginPreview();
    Q_INVOKABLE void setPreviewStartTime(double time);
    Q_INVOKABLE void setPreviewEndTime(double time);
    Q_INVOKABLE void endPreview();

    TimelineContext* context() const;
    void setContext(TimelineContext* newContext);

signals:
    void contextChanged();

private:
    static constexpr int RESIZE_AREA_WIDTH_PX = 5;
    static constexpr int MINIMUM_DRAG_LENGTH_PX = 2;

    TimelineContext* m_context = nullptr;

    double startTimePos() const;
    double endTimePos() const;

    void setStartTime(double pos);
    void setEndTime(double pos);

    playback::PlaybackRegion m_initialRegion;
    bool m_initialState;

    double m_dragStartPos = 0;
    double m_lastPos = 0;
    bool m_dragStarted = false;
    UserInputAction m_action;
};
}
