#pragma once

#include <QObject>

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "../timeline/timelinecontext.h"

namespace au::projectscene {
class SelectionController : public QObject
{
    Q_OBJECT
    Q_PROPERTY(TimelineContext * context READ timelineContext WRITE setTimelineContext NOTIFY timelineContextChanged FINAL)

    Q_PROPERTY(QList<int> selectedTracks READ selectedTracks NOTIFY selectedTracksChanged FINAL)

    muse::Inject<context::IGlobalContext> globalContext;

public:
    SelectionController(QObject* parent = nullptr);

    //! NOTE The x coordinates must match the timeline.
    //! The y coordinates must match the track view
    //! If this is not the case, then appropriate adjustments must be made.
    Q_INVOKABLE void onSelectedCoords(double x1, double y1, double x2, double y2);
    Q_INVOKABLE void resetSelection();

    TimelineContext* timelineContext() const;
    void setTimelineContext(TimelineContext* newContext);

    QList<int> selectedTracks() const;

signals:
    void tracksSelection(int trackId1, int trackId2);

    void timelineContextChanged();

    void selectedTracksChanged();

private:

    QList<int> determinateTracks(double y1, double y2) const;
    //std::pair<double, double> determinateTime(double x1, double x2) const;

    IProjectViewStatePtr viewState() const;
    std::vector<processing::TrackId> trackIdList() const;

    void setSelectedTracks(const QList<int>& tracks);

    TimelineContext* m_context = nullptr;
    QList<int> m_selectedTracks;
};
}
