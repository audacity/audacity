/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "framework/global/async/async.h"
#include "framework/global/iapplication.h"
#include "framework/global/modularity/ioc.h"

#include "context/iglobalcontext.h"
#include "trackedit/iselectioncontroller.h"
#include "trackedit/itrackeditinteraction.h"
#include "spectrogram/view/spectrogramhit.h"
#include "spectrogram/ifrequencyselectioncontroller.h"
#include "spectrogram/ispectrogramservice.h"
#include "spectrogram/iglobalspectrogramconfiguration.h"
#include "trackedit/internal/itracknavigationcontroller.h"

#include "types/projectscenetypes.h"
#include "../timeline/timelinecontext.h"

namespace au::projectscene {
class SelectionViewController : public QObject, public muse::async::Asyncable, public muse::Contextable
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
    muse::GlobalInject<muse::IApplication> application;

    muse::ContextInject<context::IGlobalContext> globalContext { this };
    muse::ContextInject<trackedit::ISelectionController> selectionController { this };
    muse::ContextInject<trackedit::ITrackeditInteraction> trackeditInteraction { this };
    muse::ContextInject<trackedit::ITrackNavigationController> trackNavigationController { this };

    muse::ContextInject<spectrogram::IFrequencySelectionController> frequencySelectionController { this };
    muse::ContextInject<spectrogram::ISpectrogramService> spectrogramService { this };

public:
    SelectionViewController(QObject* parent = nullptr);

    TimelineContext* timelineContext() const;
    void setTimelineContext(TimelineContext* newContext);

    int resistancePx() const { return m_resistancePx; }
    void setResistancePx(int value);

    Q_INVOKABLE void load();

    //! NOTE The time parameters are in seconds (audio time).
    //! The y coordinates must match the track view
    Q_INVOKABLE void onPressed(double time, double y, spectrogram::SpectrogramHit spectrogramHit = {});
    Q_INVOKABLE void onPositionChanged(double time, double y);
    Q_INVOKABLE void onReleased(double time, double y);

    Q_INVOKABLE void onSelectionHorizontalResize(double anchorTime, double draggedTime, bool completed);
    Q_INVOKABLE void startSelectionVerticalResize(spectrogram::SpectrogramHit hit, bool isTop);
    Q_INVOKABLE void updateSelectionVerticalResize(double y, bool completed);
    Q_INVOKABLE void cancelSpectrogramEdit();

    Q_INVOKABLE void cancelSelectionGesture();
    Q_INVOKABLE void selectTrackAudioData(double y);
    Q_INVOKABLE void selectItemData(const TrackItemKey& key);

    Q_INVOKABLE void resetSelectedItems();
    Q_INVOKABLE void resetSelectedClips();
    Q_INVOKABLE void resetSelectedLabel();
    Q_INVOKABLE void resetDataSelection();
    Q_INVOKABLE bool isLeftSelection(double time) const;

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

private:

    IProjectViewStatePtr viewState() const;
    trackedit::TrackIdList trackIdList() const;

    bool doOnPositionChanged(double time, double y);
    void setSelectionTimes(double time1, double time2, bool complete);
    double snapTime(double time) const;
    void setFrequencySelectionEdge(double y, bool complete = true, uintptr_t handle = 0); // If handle is 0, sets both

    Qt::KeyboardModifiers keyboardModifiers() const;

    bool isProjectOpened() const;

    TimelineContext* m_context = nullptr;
    int m_resistancePx = 0;
    QMetaObject::Connection m_autoScrollConnection;

    bool m_selectionStarted = false;
    bool m_selectionThresholdCrossed = false;
    bool m_selectionActive = false;
    bool m_selectionEditInProgress = false;
    bool m_verticalSelectionEditInProgress = false;

    double m_selectionStartTime = 0.0;
    double m_startY = 0.0;
    double m_autoScrollLastX = 0.0;
    double m_autoScrollLastY = 0.0;
    double m_horizontalResizeAnchorTime = 0.0;

    double spectrogramHitFrequency(const spectrogram::SpectrogramHit& hit, double y) const;
    bool isInExtendedSpectrogram(const spectrogram::SpectrogramHit& hit, double y) const;

    std::optional<const spectrogram::SpectrogramHit> m_spectrogramHit;
    uintptr_t m_frequencyEdgeHandle = 0;
};
}
