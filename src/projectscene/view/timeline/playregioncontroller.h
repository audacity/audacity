/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <QObject>

#include "modularity/ioc.h"
#include "context/iuicontextresolver.h"
#include "context/iglobalcontext.h"
#include "playback/iplaybackcontroller.h"

#include "timelinecontext.h"

namespace au::projectscene {
class PlayRegionController : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(TimelineContext * context READ context WRITE setContext NOTIFY contextChanged FINAL)
    Q_PROPERTY(double guidelinePosition READ guidelinePosition NOTIFY guidelinePositionChanged FINAL)
    Q_PROPERTY(bool guidelineVisible READ guidelineVisible NOTIFY guidelineVisibleChanged FINAL)

    muse::Inject<playback::IPlaybackController> playbackController;
    muse::Inject<context::IUiContextResolver> uicontextResolver;
    muse::Inject<context::IGlobalContext> globalContext;

    enum class UserInputAction {
        None,
        CreateRegion,
        ResizeStart,
        ResizeEnd,
        Drag
    };

public:
    PlayRegionController(QObject* parent = nullptr);

    Q_INVOKABLE void init();

    Q_INVOKABLE void startInteraction(double pos, bool ctrlPressed);
    Q_INVOKABLE void finishInteraction(double pos);
    Q_INVOKABLE void updatePosition(double pos);

    Q_INVOKABLE void beginPreview();
    Q_INVOKABLE void setPreviewStartTime(double time);
    Q_INVOKABLE void setPreviewEndTime(double time);
    Q_INVOKABLE void endPreview();

    TimelineContext* context() const;
    void setContext(TimelineContext* newContext);

    double guidelinePosition() const;
    bool guidelineVisible() const;

signals:
    void contextChanged() const;
    void guidelinePositionChanged() const;
    void guidelineVisibleChanged() const;

private:
    static constexpr int RESIZE_AREA_WIDTH_PX = 5;
    static constexpr int MINIMUM_DRAG_LENGTH_PX = 2;

    void updateIsActive();

    double startPos() const;
    double endPos() const;

    void setStartPos(double pos);
    void setEndPos(double pos);

    void updateSnapGuideline(double pos);
    void resetSnapGuideline();
    double calculateSnappedPosition(double pos) const;
    double setGuidelinePosition(double pos) const;

    void handleDrag(double pos);

    TimelineContext* m_context = nullptr;

    playback::PlaybackRegion m_initialRegion;
    bool m_initialState;

    double m_dragStartPos = 0;
    double m_lastPos = 0;
    double m_snapGuidelinePos = 0;

    bool m_dragStarted = false;
    UserInputAction m_action = UserInputAction::None;

    bool m_isActive = false;
};
}
