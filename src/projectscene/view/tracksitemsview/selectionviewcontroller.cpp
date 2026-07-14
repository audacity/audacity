/*
* Audacity: A Digital Audio Editor
*/
#include <QApplication>

#include <cmath>

#include "selectionviewcontroller.h"

#include "spectrogram/spectrogramtypes.h"
#include "spectrogram/view/spectrogramhit.h"
#include "framework/global/log.h"

using namespace au::projectscene;
using namespace au::project;
using namespace au::trackedit;

constexpr double MIN_SELECTION_PX = 1.0;
constexpr double SELECTION_DRAG_THRESHOLD_PX = 5.0;

SelectionViewController::SelectionViewController(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

void SelectionViewController::load()
{
    connect(qApp, &QApplication::applicationStateChanged, this, [this](Qt::ApplicationState state){
        if (state != Qt::ApplicationActive) {
            //Application lost focus, end any selection in progress
            onReleased(m_selectionStartTime, m_startY);
        }
    });

    spectrogramConfiguration()->spectralSelectionEnabledChanged().onReceive(this, [this] (auto) {
        emit spectralSelectionEnabledChanged();
    });
}

void SelectionViewController::onPressed(double time, double y, spectrogram::SpectrogramHit spectrogramHit)
{
    if (!isProjectOpened()) {
        return;
    }

    IProjectViewStatePtr vs = viewState();
    if (!vs) {
        return;
    }

    m_autoScrollLastX = m_context->timeToPosition(time);
    m_autoScrollLastY = y;

    Qt::KeyboardModifiers modifiers = keyboardModifiers();

    m_selectionStarted = true;
    {
        const bool modifierHeld = modifiers.testFlag(Qt::ShiftModifier) || modifiers.testFlag(Qt::ControlModifier);
        m_selectionThresholdCrossed = modifierHeld;
        //! NOTE: do not update start point when user holds Shift or Ctrl
        if (!modifierHeld) {
            m_selectionStartTime = time;
            m_startY = y;
            selectionController()->setSelectionStartTime(m_selectionStartTime);
        } else {
            m_selectionStartTime = selectionController()->selectionStartTime();
        }
    }
    emit selectionStarted();
    emit selectionInProgressChanged();
    resetDataSelection();

    TrackIdList tracks;
    if (modifiers.testFlag(Qt::ControlModifier)) {
        tracks = selectionController()->selectedTracks();
        TrackIdList newTracks = vs->tracksInRange(y, y);
        if (!newTracks.empty()) {
            const TrackId clickedTrack = newTracks.at(0);
            auto it = std::find(tracks.begin(), tracks.end(), clickedTrack);
            if (it == tracks.end()) {
                tracks.push_back(clickedTrack);
            } else {
                tracks.erase(it);
            }
        }
    } else {
        tracks = vs->tracksInRange(m_startY, y);
    }

    if (!tracks.empty()) {
        trackNavigationController()->setFocusedTrack(tracks.at(0), false /*highlight*/);
    } else {
        trackNavigationController()->setFocusedItem({});
    }
    selectionController()->setSelectedTracks(tracks, true);

    if (modifiers.testFlag(Qt::ShiftModifier) || modifiers.testFlag(Qt::ControlModifier)) {
        double time1 = m_selectionStartTime;
        double time2 = time;
        if (time1 > time2) {
            std::swap(time1, time2);
        }

        setSelectionActive(true);

        selectionController()->setDataSelectedStartTime(snapTime(time1), false);
        selectionController()->setDataSelectedEndTime(snapTime(time2), false);
    }

    m_spectrogramHit.reset();
    if (spectralSelectionEnabled() && spectrogramHit.trackId >= 0) {
        // Don't begin a spectral selection until we've actually moved.
        m_spectrogramHit.emplace(spectrogramHit);
    }
    emit pressedSpectrogramChanged();

    viewState()->updateItemsBoundaries(false);

    m_autoScrollConnection = connect(m_context, &TimelineContext::frameTimeChanged, [this]() {
        doOnPositionChanged(m_context->positionToTime(m_autoScrollLastX), m_autoScrollLastY);
    });
}

namespace {
double clampToSpectrogram(const au::spectrogram::SpectrogramHit& hit, double y)
{
    return std::clamp(y, hit.spectrogramY, hit.spectrogramY + hit.spectrogramHeight);
}
}

void SelectionViewController::onPositionChanged(double time, double y)
{
    if (m_spectrogramHit && isInExtendedSpectrogram(*m_spectrogramHit, y)) {
        y = clampToSpectrogram(*m_spectrogramHit, y);
    }
    if (doOnPositionChanged(time, y) && m_spectrogramHit) {
        if (m_frequencyEdgeHandle == 0) {
            const auto frequency = spectrogramHitFrequency(*m_spectrogramHit, m_startY);
            m_frequencyEdgeHandle = frequencySelectionController()->beginSelection(m_spectrogramHit->trackId, frequency);
        }
        setFrequencySelectionEdge(y, false, m_frequencyEdgeHandle);
    }
}

bool SelectionViewController::doOnPositionChanged(double time, double y)
{
    if (!isProjectOpened()) {
        return false;
    }

    IProjectViewStatePtr vs = viewState();
    if (!vs) {
        return false;
    }

    if (!m_selectionStarted) {
        return false;
    }

    Qt::KeyboardModifiers modifiers = keyboardModifiers();

    time = std::max(time, 0.0);
    m_autoScrollLastX = m_context->timeToPosition(time);
    m_autoScrollLastY = y;
    m_context->startAutoScroll(time);

    if (!m_selectionThresholdCrossed) {
        const double startXPx = m_context->timeToPosition(m_selectionStartTime);
        if (std::abs(m_autoScrollLastX - startXPx) < SELECTION_DRAG_THRESHOLD_PX
            && std::abs(y - m_startY) < SELECTION_DRAG_THRESHOLD_PX) {
            return false;
        }
        m_selectionThresholdCrossed = true;
    }

    // tracks
    TrackIdList tracks;
    if (modifiers.testFlag(Qt::ControlModifier)) {
        tracks = selectionController()->selectedTracks();
    } else {
        tracks = vs->tracksInRange(m_startY, y);
    }
    selectionController()->setSelectedTracks(tracks, false);

    // time
    double time1 = m_selectionStartTime;
    double time2 = time;
    if (time1 > time2) {
        std::swap(time1, time2);
    }

    setSelectionTimes(time1, time2, false);

    return true;
}

void SelectionViewController::onReleased(double time, double y)
{
    if (!isProjectOpened()) {
        return;
    }

    setFrequencySelectionEdge(y, true, m_frequencyEdgeHandle);
    m_spectrogramHit.reset();
    m_frequencyEdgeHandle = 0;

    IProjectViewStatePtr vs = viewState();
    if (!vs) {
        return;
    }

    if (!m_selectionStarted) {
        return;
    }

    Qt::KeyboardModifiers modifiers = keyboardModifiers();

    m_selectionStarted = false;

    time = std::max(time, 0.0);
    m_autoScrollLastX = m_context->timeToPosition(time);
    m_autoScrollLastY = y;
    m_context->stopAutoScroll();
    disconnect(m_autoScrollConnection);

    emit selectionInProgressChanged();

    double time1 = m_selectionStartTime;
    double time2 = time;
    if (time1 > time2) {
        std::swap(time1, time2);
    }

    TrackIdList tracks;
    if (modifiers.testFlag(Qt::ControlModifier)) {
        tracks = selectionController()->selectedTracks();
    } else {
        tracks = vs->tracksInRange(m_startY, y);
    }

    const double minSelectionTime = m_context->positionToTime(MIN_SELECTION_PX) - m_context->positionToTime(0.0);
    if (!m_selectionThresholdCrossed || (time2 - time1) < minSelectionTime) {
        // Click without drag (or a drag that never crossed the threshold)
        if (!tracks.empty()) {
            selectionController()->setSelectedTracks(tracks);
        } else {
            selectionController()->resetSelectedTracks();
        }
        setSelectionTimes(time1, time1, true);
        return;
    }

    if (tracks.empty()) {
        selectionController()->resetSelectedTracks();
        setSelectionTimes(time1, time2, true);
        return;
    }

    setSelectionActive(true);

    if (m_startY < y) {
        trackNavigationController()->setFocusedTrack(tracks.back());
    } else {
        trackNavigationController()->setFocusedTrack(tracks.front());
    }
    selectionController()->setSelectedTracks(tracks, true);

    // time
    setSelectionTimes(time1, time2, true);
}

void SelectionViewController::onSelectionHorizontalResize(double time1, double time2, bool completed)
{
    if (!isProjectOpened()) {
        return;
    }

    if (time1 > time2) {
        std::swap(time1, time2);
    }

    setSelectionTimes(time1, time2, completed);
    m_selectionEditInProgress = !completed;
    emit selectionEditInProgressChanged();
}

void SelectionViewController::startSelectionVerticalResize(spectrogram::SpectrogramHit hit, bool isTop)
{
    IF_ASSERT_FAILED(hit.trackId >= 0) {
        return;
    }
    m_spectrogramHit.emplace(hit);
    m_frequencyEdgeHandle
        = isTop ? frequencySelectionController()->endFrequencyHandle() : frequencySelectionController()->startFrequencyHandle();
    m_verticalSelectionEditInProgress = true;
    emit verticalSelectionEditInProgressChanged();
}

void SelectionViewController::updateSelectionVerticalResize(double y, bool completed)
{
    if (!m_verticalSelectionEditInProgress || !m_spectrogramHit || m_frequencyEdgeHandle == 0) {
        return;
    }

    auto frequency = spectrogram::SelectionInfo::UndefinedFrequency;
    if (isInExtendedSpectrogram(*m_spectrogramHit, y)) {
        frequency = spectrogramHitFrequency(*m_spectrogramHit, clampToSpectrogram(*m_spectrogramHit, y));
    }

    frequencySelectionController()->setHandleFrequency(frequency, completed, m_frequencyEdgeHandle);
}

void SelectionViewController::cancelSpectrogramEdit()
{
    frequencySelectionController()->resetFrequencySelection();

    if (!m_verticalSelectionEditInProgress) {
        return;
    }

    m_verticalSelectionEditInProgress = false;
    m_frequencyEdgeHandle = 0;
    m_spectrogramHit.reset();
    emit verticalSelectionEditInProgressChanged();
    emit pressedSpectrogramChanged();
}

void SelectionViewController::cancelSelectionGesture()
{
    if (!m_selectionStarted) {
        return;
    }

    m_selectionStarted = false;
    m_context->stopAutoScroll();
    disconnect(m_autoScrollConnection);
    emit selectionInProgressChanged();
}

void SelectionViewController::selectTrackAudioData(double y)
{
    if (!isProjectOpened()) {
        return;
    }

    IProjectViewStatePtr vs = viewState();
    if (!vs) {
        return;
    }

    const std::vector<TrackId> tracks = vs->tracksInRange(m_startY, y);
    if (tracks.empty()) {
        return;
    }

    cancelSelectionGesture();

    selectionController()->setSelectedTrackAudioData(tracks.at(0));
}

void SelectionViewController::selectItemData(const TrackItemKey& key)
{
    if (!isProjectOpened()) {
        return;
    }

    auto prj = globalContext()->currentTrackeditProject();
    auto track = prj->track(key.key.trackId);
    if (!track.has_value()) {
        LOGW() << "Track not found: " << key.key.trackId;
        return;
    }

    cancelSelectionGesture();

    if (track->type == trackedit::TrackType::Label) {
        selectionController()->setSelectedLabels(trackedit::LabelKeyList({ key.key }));
    } else {
        selectionController()->setSelectedClips(trackedit::ClipKeyList({ key.key }));
    }
}

void SelectionViewController::resetSelectedItems()
{
    if (!isProjectOpened()) {
        return;
    }

    selectionController()->resetSelectedClips();
    selectionController()->resetSelectedLabels();
}

void SelectionViewController::resetSelectedClips()
{
    if (!isProjectOpened()) {
        return;
    }

    selectionController()->resetSelectedClips();
}

void SelectionViewController::resetSelectedLabel()
{
    if (!isProjectOpened()) {
        return;
    }

    selectionController()->resetSelectedLabels();
}

void SelectionViewController::resetDataSelection()
{
    if (!isProjectOpened()) {
        return;
    }
    setSelectionActive(false);
    selectionController()->resetDataSelection();
    frequencySelectionController()->resetFrequencySelection();
}

bool SelectionViewController::isLeftSelection(double time) const
{
    if (!isProjectOpened()) {
        return false;
    }

    return m_selectionStartTime > time;
}

IProjectViewStatePtr SelectionViewController::viewState() const
{
    IAudacityProjectPtr prj = globalContext()->currentProject();
    return prj ? prj->viewState() : nullptr;
}

TrackIdList SelectionViewController::trackIdList() const
{
    ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    return prj ? prj->trackIdList() : TrackIdList();
}

Qt::KeyboardModifiers SelectionViewController::keyboardModifiers() const
{
    Qt::KeyboardModifiers modifiers = QApplication::keyboardModifiers();

    //! NOTE: always treat simultaneously pressed Ctrl and Shift as Ctrl
    if (modifiers.testFlag(Qt::ShiftModifier) && modifiers.testFlag(Qt::ControlModifier)) {
        modifiers = Qt::ControlModifier;
    }

    return modifiers;
}

bool SelectionViewController::isProjectOpened() const
{
    return globalContext()->currentProject() != nullptr;
}

TimelineContext* SelectionViewController::timelineContext() const
{
    return m_context;
}

void SelectionViewController::setTimelineContext(TimelineContext* newContext)
{
    if (m_context == newContext) {
        return;
    }
    m_context = newContext;
    emit timelineContextChanged();
}

void SelectionViewController::setResistancePx(int value)
{
    if (m_resistancePx == value) {
        return;
    }
    m_resistancePx = value;
    emit resistancePxChanged();
}

bool SelectionViewController::selectionActive() const
{
    return m_selectionActive;
}

bool SelectionViewController::selectionEditInProgress() const
{
    return m_selectionEditInProgress;
}

bool SelectionViewController::verticalSelectionEditInProgress() const
{
    return m_verticalSelectionEditInProgress;
}

bool SelectionViewController::selectionInProgress() const
{
    return m_selectionStarted;
}

bool SelectionViewController::spectralSelectionEnabled() const
{
    return spectrogramConfiguration()->spectralSelectionEnabled();
}

QVariantMap SelectionViewController::pressedSpectrogram() const
{
    const auto trackId = m_spectrogramHit ? m_spectrogramHit->trackId : -1;
    const auto channel = m_spectrogramHit ? m_spectrogramHit->channel : -1;
    return {
        { "trackId", trackId },
        { "channel", channel },
    };
}

void SelectionViewController::setSelectionActive(bool newSelectionActive)
{
    if (m_selectionActive == newSelectionActive) {
        return;
    }
    m_selectionActive = newSelectionActive;
    emit selectionActiveChanged();
}

void SelectionViewController::setSelectionTimes(double time1, double time2, bool complete)
{
    selectionController()->setDataSelectedStartTime(snapTime(time1), complete);
    selectionController()->setDataSelectedEndTime(snapTime(time2), complete);
}

double SelectionViewController::snapTime(double time) const
{
    return m_context ? m_context->applyDetectedSnap(time) : time;
}

double SelectionViewController::spectrogramHitFrequency(const spectrogram::SpectrogramHit& hit, double y) const
{
    return spectrogramService()->yToFrequency(hit.trackId, y - hit.spectrogramY, hit.spectrogramHeight);
}

void SelectionViewController::setFrequencySelectionEdge(double y, bool complete, uintptr_t handle)
{
    if (!m_spectrogramHit) {
        return;
    }

    auto frequency = spectrogram::SelectionInfo::UndefinedFrequency;
    if (isInExtendedSpectrogram(*m_spectrogramHit, y)) {
        frequency = spectrogramHitFrequency(*m_spectrogramHit, y);
    }

    frequencySelectionController()->setHandleFrequency(frequency, complete, handle);
}

bool SelectionViewController::isInExtendedSpectrogram(const spectrogram::SpectrogramHit& hit, double y) const
{
    return hit.spectrogramY - m_resistancePx <= y && y <= hit.spectrogramY + hit.spectrogramHeight + m_resistancePx;
}
