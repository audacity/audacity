/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "framework/global/async/async.h"
#include "framework/global/modularity/ioc.h"

#include "context/iglobalcontext.h"
#include "trackedit/iselectioncontroller.h"
#include "trackedit/itrackeditinteraction.h"
#include "spectrogram/view/spectrogramhit.h"
#include "spectrogram/ispectrogramservice.h"
#include "spectrogram/iglobalspectrogramconfiguration.h"
#include "trackedit/internal/itracknavigationcontroller.h"

#include "types/projectscenetypes.h"
#include "../timeline/timelinecontext.h"

namespace au::projectscene {
class SelectionViewController : public QObject, public muse::async::Asyncable, public muse::Injectable
{
    Q_OBJECT
    Q_PROPERTY(TimelineContext * context READ timelineContext WRITE setTimelineContext NOTIFY timelineContextChanged FINAL)
    Q_PROPERTY(int resistancePx READ resistancePx WRITE setResistancePx NOTIFY resistancePxChanged FINAL)

    Q_PROPERTY(bool selectionActive READ selectionActive NOTIFY selectionActiveChanged FINAL)
    Q_PROPERTY(bool selectionEditInProgress READ selectionEditInProgress NOTIFY selectionEditInProgressChanged FINAL)
    Q_PROPERTY(bool verticalSelectionEditInProgress READ verticalSelectionEditInProgress NOTIFY verticalSelectionEditInProgressChanged FINAL)
    Q_PROPERTY(bool selectionInProgress READ selectionInProgress NOTIFY selectionInProgressChanged FINAL)
    Q_PROPERTY(bool spectralSelectionEnabled READ spectralSelectionEnabled NOTIFY spectralSelectionEnabledChanged FINAL)
    Q_PROPERTY(QVariantMap pressedSpectrogram READ pressedSpectrogram NOTIFY pressedSpectrogramChanged FINAL)

    muse::GlobalInject<spectrogram::IGlobalSpectrogramConfiguration> spectrogramConfiguration;

    muse::Inject<context::IGlobalContext> globalContext { this };
    muse::Inject<trackedit::ISelectionController> selectionController { this };
    muse::Inject<trackedit::ITrackeditInteraction> trackeditInteraction { this };
    muse::Inject<spectrogram::ISpectrogramService> spectrogramService { this };
    muse::Inject<trackedit::ITrackNavigationController> trackNavigationController { this };

public:
    SelectionViewController(QObject* parent = nullptr);

    TimelineContext* timelineContext() const;
    void setTimelineContext(TimelineContext* newContext);

    int resistancePx() const { return m_resistancePx; }
    void setResistancePx(int value);

    Q_INVOKABLE void load();

    //! NOTE The x coordinates must match the timeline.
    //! The y coordinates must match the track view
    //! If this is not the case, then appropriate adjustments must be made.
    Q_INVOKABLE void onPressed(double x, double y, spectrogram::SpectrogramHit spectrogramHit = {});
    Q_INVOKABLE void onPositionChanged(double x, double y);
    Q_INVOKABLE void onReleased(double x, double y);

    Q_INVOKABLE void onSelectionHorizontalResize(double x, double x2, bool completed);
    Q_INVOKABLE void startSelectionVerticalResize(spectrogram::SpectrogramHit hit);
    Q_INVOKABLE void updateSelectionVerticalResize(double y1, double y2, bool completed);

    Q_INVOKABLE void selectTrackAudioData(double y);
    Q_INVOKABLE void selectItemData(const TrackItemKey& key);

    Q_INVOKABLE void resetSelectedItems();
    Q_INVOKABLE void resetSelectedClips();
    Q_INVOKABLE void resetSelectedLabel();
    Q_INVOKABLE void resetDataSelection();
    Q_INVOKABLE bool isLeftSelection(double x) const;

    bool selectionActive() const;
    bool selectionEditInProgress() const;
    bool verticalSelectionEditInProgress() const;
    bool selectionInProgress() const;
    bool spectralSelectionEnabled() const;
    void setSelectionActive(bool newSelectionActive);
    QVariantMap pressedSpectrogram() const;

signals:
    void timelineContextChanged();
    void resistancePxChanged();
    void selectionActiveChanged();
    void selectionEditInProgressChanged();
    void verticalSelectionEditInProgressChanged();
    void selectionInProgressChanged();
    void spectralSelectionEnabledChanged();
    void pressedSpectrogramChanged();

    void selectionStarted();
    void selectionChanged(QPointF p1, QPointF p2);
    void selectionEnded(QPointF p1, QPointF p2);

private:

    IProjectViewStatePtr viewState() const;
    trackedit::TrackIdList trackIdList() const;

    bool doOnPositionChanged(double x, double y);
    void setSelection(double x1, double x2, bool complete);
    void setFrequencySelection(double y1, double y2);

    Qt::KeyboardModifiers keyboardModifiers() const;

    bool isProjectOpened() const;

    TimelineContext* m_context = nullptr;
    int m_resistancePx = 0;
    QMetaObject::Connection m_autoScrollConnection;

    bool m_selectionStarted = false;
    bool m_selectionActive = false;
    bool m_selectionEditInProgress = false;
    bool m_verticalSelectionEditInProgress = false;
    QPointF m_startPoint;
    QPointF m_lastPoint;

    double spectrogramHitFrequency(const spectrogram::SpectrogramHit& hit, double y) const;
    bool isInExtendedSpectrogram(const spectrogram::SpectrogramHit& hit, double y) const;

    std::optional<const spectrogram::SpectrogramHit> m_spectrogramHit;
};
}
