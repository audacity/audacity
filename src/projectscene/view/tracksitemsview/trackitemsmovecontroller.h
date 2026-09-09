/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>
#include <QPointer>

#include "async/asyncable.h"
#include "context/iglobalcontext.h"
#include "trackedit/iprojecthistory.h"
#include "trackedit/iselectioncontroller.h"
#include "trackedit/itrackeditinteraction.h"
#include "trackedit/itracksinteraction.h"
#include "trackedit/itracksviewrequestsservice.h"
#include "trackedit/internal/itracknavigationcontroller.h"
#include "../timeline/timelinecontext.h"

namespace au::projectscene {
class TrackItemsMoveController : public QObject, public muse::async::Asyncable, public muse::Contextable
{
    Q_OBJECT

    Q_PROPERTY(TimelineContext * context READ timelineContext WRITE setTimelineContext NOTIFY contextChanged FINAL)
    Q_PROPERTY(bool active READ active NOTIFY activeChanged FINAL)
    Q_PROPERTY(bool keyboardActive READ keyboardActive NOTIFY activeChanged FINAL)

    muse::ContextInject<context::IGlobalContext> globalContext{ this };
    muse::ContextInject<trackedit::ISelectionController> selectionController{ this };
    muse::ContextInject<trackedit::ITrackeditInteraction> trackeditInteraction{ this };
    muse::ContextInject<trackedit::ITracksInteraction> tracksInteraction{ this };
    muse::ContextInject<trackedit::IProjectHistory> projectHistory{ this };
    muse::ContextInject<trackedit::ITracksViewRequestsService> tracksViewRequestsService{ this };
    muse::ContextInject<trackedit::ITrackNavigationController> trackNavigationController{ this };

public:
    explicit TrackItemsMoveController(QObject* parent = nullptr);
    ~TrackItemsMoveController() override;

    TimelineContext* timelineContext() const;
    void setTimelineContext(TimelineContext* context);

    Q_INVOKABLE void init();
    Q_INVOKABLE void start(const TrackItemKey& key);
    Q_INVOKABLE void update();
    Q_INVOKABLE TrackItemKey finish();
    Q_INVOKABLE bool cancel();

    bool active() const;
    bool keyboardActive() const;
    bool isDragged(const trackedit::TrackItemKey& key) const;
    double timeOffset() const;
    trackedit::TrackItemKeyList itemsOnTrack(trackedit::TrackId trackId) const;

signals:
    void contextChanged();
    void activeChanged();
    void previewChanged();
    void guidelineChanged(double time);
    void keyboardTrackChanged(au::trackedit::TrackId trackId);

private:
    friend class TrackItemsMoveControllerTests;

    void start(const TrackItemKey& key, bool keyboard);
    void moveByKeyboard(double timeOffset, int trackOffset);
    double pointerTimeOffset(double start, double end) const;
    int pointerTrackOffset() const;
    void updatePreview(double timeOffset, int trackOffset);
    void endInteraction();

    QPointer<TimelineContext> m_context;
    IProjectViewStatePtr m_viewState;
    trackedit::ITrackeditProjectPtr m_project;
    trackedit::TrackItemKey m_sourceKey;
    trackedit::ClipKeyList m_clips;
    trackedit::LabelKeyList m_labels;
    double m_startTime = 0.0;
    double m_endTime = 0.0;
    double m_timeOffset = 0.0;
    int m_trackOffset = 0;
    size_t m_originalTrackCount = 0;
    bool m_sourceIsLabel = false;
    bool m_rangeSelection = false;
    bool m_keyboardMove = false;
    bool m_moved = false;
    bool m_updating = false;
};
}
