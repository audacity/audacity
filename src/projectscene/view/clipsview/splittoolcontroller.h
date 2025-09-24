/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <QObject>

#include "modularity/ioc.h"
#include "context/iuicontextresolver.h"
#include "../timeline/timelinecontext.h"

namespace au::projectscene {
class TapHoldShortcut;

class SplitToolController : public QObject, public muse::actions::Actionable, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(TimelineContext * context READ context WRITE setContext NOTIFY contextChanged FINAL)
    Q_PROPERTY(bool active READ active NOTIFY activeChanged FINAL)
    Q_PROPERTY(bool singleTrack READ singleTrack NOTIFY singleTrackChanged FINAL)
    Q_PROPERTY(int hoveredTrack READ hoveredTrack WRITE setHoveredTrack NOTIFY hoveredTrackChanged FINAL)
    Q_PROPERTY(bool clipHovered READ clipHovered WRITE setClipHovered NOTIFY clipHoveredChanged FINAL)
    Q_PROPERTY(double guidelinePosition READ guidelinePosition NOTIFY guidelinePositionChanged FINAL)
    Q_PROPERTY(bool guidelineVisible READ guidelineVisible NOTIFY guidelineVisibleChanged FINAL)

    muse::Inject<muse::actions::IActionsDispatcher> dispatcher;
    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<context::IUiContextResolver> uicontextResolver;

public:
    SplitToolController(QObject* parent = nullptr);
    ~SplitToolController();

    Q_INVOKABLE void init(QObject* root);

    Q_INVOKABLE void mouseDown(double pos);
    Q_INVOKABLE void mouseUp(double pos);
    Q_INVOKABLE void mouseMove(double pos);

    TimelineContext* context() const;
    void setContext(TimelineContext* newContext);

    double guidelinePosition() const;
    bool guidelineVisible() const;

    bool active() const;
    void setActive(bool newActive);

    void setGuidelinePos(double newGuidelinePos);

    bool clipHovered() const;
    void setClipHovered(bool newClipHovered);

    int hoveredTrack() const;
    void setHoveredTrack(int newHoveredTrack);

    bool singleTrack() const;
    void setSingleTrack(bool enabled);

signals:
    void contextChanged();
    void guidelinePositionChanged();
    void guidelineVisibleChanged();
    void activeChanged();
    void clipHoveredChanged();
    void hoveredTrackChanged();

    void singleTrackChanged();

private:
    const int MIN_DOUBLE_SPLIT_DISTANCE = 10;

    bool eventFilter(QObject* obj, QEvent* event) override;

    void updateGuideline(double pos);
    void updateCursor();

    void overrideCursor();
    void restoreCursor();

    void splitTracksAt(trackedit::TrackIdList ids, double t);
    void doSplit();

    std::unique_ptr<TapHoldShortcut> m_shortcut;

    TimelineContext* m_context = nullptr;
    QCursor m_cursor;

    double m_guidelinePos = 0;
    double m_splitStartPos = 0;

    bool m_active = false;
    bool m_prePressState = false;
    bool m_cursorOverriden = false;
    bool m_clipHovered = false;
    bool m_singleTrack = true;
    int m_hoveredTrack = -1;
};
}
