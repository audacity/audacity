#pragma once

#include <QObject>

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "processing/iprocessingselectioncontroller.h"

#include "../timeline/timelinecontext.h"

namespace au::projectscene {
class SelectionViewController : public QObject
{
    Q_OBJECT
    Q_PROPERTY(TimelineContext * context READ timelineContext WRITE setTimelineContext NOTIFY timelineContextChanged FINAL)

    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<processing::IProcessingSelectionController> processingSelectionController;

public:
    SelectionViewController(QObject* parent = nullptr);

    TimelineContext* timelineContext() const;
    void setTimelineContext(TimelineContext* newContext);

    QList<int> selectedTracks() const;
    double selectedStartTime() const;
    double selectedEndTime() const;

    //! NOTE The x coordinates must match the timeline.
    //! The y coordinates must match the track view
    //! If this is not the case, then appropriate adjustments must be made.
    Q_INVOKABLE void onSelectedCoords(double x1, double y1, double x2, double y2);
    Q_INVOKABLE void resetSelection();

signals:
    void timelineContextChanged();

private:

    IProjectViewStatePtr viewState() const;
    std::vector<processing::TrackId> trackIdList() const;

    std::vector<processing::TrackId> determinateTracks(double y1, double y2) const;

    TimelineContext* m_context = nullptr;
};
}
