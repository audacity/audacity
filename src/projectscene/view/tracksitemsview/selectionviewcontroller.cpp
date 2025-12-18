/*
* Audacity: A Digital Audio Editor
*/
#include "selectionviewcontroller.h"

#include "log.h"

using namespace au::projectscene;
using namespace au::project;
using namespace au::trackedit;

//! NOTE: sync with ItemsSelection.qml minSelection
constexpr double MIN_SELECTION_PX = 1.0;

SelectionViewController::SelectionViewController(QObject* parent)
    : QObject(parent)
{
}

void SelectionViewController::load()
{
    connect(qApp, &QApplication::applicationStateChanged, this, [this](Qt::ApplicationState state){
        if (state != Qt::ApplicationActive) {
            //Application lost focus, end any selection in progress
            onReleased(m_startPoint.x(), m_startPoint.y());
        }
    });
}

void SelectionViewController::onPressed(double x, double y)
{
    if (!isProjectOpened()) {
        return;
    }

    IProjectViewStatePtr vs = viewState();
    if (!vs) {
        return;
    }

    m_lastPoint = QPointF(x, y);

    Qt::KeyboardModifiers modifiers = keyboardModifiers();

    m_selectionStarted = true;
    //! NOTE: do not update start point when user holds Shift or Ctrl
    if (!(modifiers.testFlag(Qt::ShiftModifier) || modifiers.testFlag(Qt::ControlModifier))) {
        m_startPoint = QPointF(x, y);
        selectionController()->setSelectionStartTime(m_context->positionToTime(m_startPoint.x()));
    }
    emit selectionStarted();
    emit selectionInProgressChanged();
    resetDataSelection();

    TrackIdList tracks;
    if (modifiers.testFlag(Qt::ControlModifier)) {
        tracks = selectionController()->selectedTracks();
        TrackIdList newTracks = vs->tracksInRange(y, y);
        if (!newTracks.empty()) {
            if (!muse::contains(tracks, newTracks.at(0))) {
                tracks.push_back(newTracks.at(0));
            }
        }
    } else {
        tracks = vs->tracksInRange(m_startPoint.y(), y);
    }

    if (!tracks.empty()) {
        selectionController()->setFocusedTrack(tracks.at(0));
    }
    selectionController()->setSelectedTracks(tracks, true);

    if (modifiers.testFlag(Qt::ShiftModifier) || modifiers.testFlag(Qt::ControlModifier)) {
        double x1 = m_startPoint.x();
        double x2 = x;
        if (x1 > x2) {
            std::swap(x1, x2);
        }

        setSelectionActive(true);

        selectionController()->setDataSelectedStartTime(m_context->positionToTime(x1, true /*withSnap*/), false);
        selectionController()->setDataSelectedEndTime(m_context->positionToTime(x2, true /*withSnap*/), false);
    }

    viewState()->updateItemsBoundaries(true);

    m_autoScrollConnection = connect(m_context, &TimelineContext::frameTimeChanged, [this](){
        onPositionChanged(m_lastPoint.x(), m_lastPoint.y());
    });
}

void SelectionViewController::onPositionChanged(double x, double y)
{
    if (!isProjectOpened()) {
        return;
    }

    IProjectViewStatePtr vs = viewState();
    if (!vs) {
        return;
    }

    if (!m_selectionStarted) {
        return;
    }

    Qt::KeyboardModifiers modifiers = keyboardModifiers();

    x = std::max(x, 0.0);
    m_lastPoint = QPointF(x, y);
    m_context->startAutoScroll(m_context->positionToTime(x));

    //! NOTE: update m_startPoint in case frameTime changed
    m_startPoint.setX(m_context->timeToPosition(selectionController()->selectionStartTime()));

    // point
    emit selectionChanged(m_startPoint, QPointF(x, y));

    // tracks
    TrackIdList tracks;
    if (modifiers.testFlag(Qt::ControlModifier)) {
        tracks = selectionController()->selectedTracks();
    } else {
        tracks = vs->tracksInRange(m_startPoint.y(), y);
    }
    selectionController()->setSelectedTracks(tracks, false);

    // time
    double x1 = m_startPoint.x();
    double x2 = x;
    if (x1 > x2) {
        std::swap(x1, x2);
    }

    setSelection(x1, x2, false);
    
    // spectral selection
    double y1 = m_startPoint.y();
    double y2 = y;
    setSpectralSelection(y1, y2, false);
}

void SelectionViewController::onReleased(double x, double y)
{
    if (!isProjectOpened()) {
        return;
    }

    IProjectViewStatePtr vs = viewState();
    if (!vs) {
        return;
    }

    if (!m_selectionStarted) {
        return;
    }

    Qt::KeyboardModifiers modifiers = keyboardModifiers();

    m_selectionStarted = false;

    x = std::max(x, 0.0);
    m_lastPoint = QPointF(x, y);
    m_context->stopAutoScroll();
    disconnect(m_autoScrollConnection);

    // point
    emit selectionEnded(m_startPoint, QPointF(x, y));
    emit selectionInProgressChanged();

    double x1 = m_startPoint.x();
    double x2 = x;
    if (x1 > x2) {
        std::swap(x1, x2);
    }

    TrackIdList tracks;
    if (modifiers.testFlag(Qt::ControlModifier)) {
        tracks = selectionController()->selectedTracks();
    } else {
        tracks = vs->tracksInRange(m_startPoint.y(), y);
    }

    if ((x2 - x1) < MIN_SELECTION_PX) {
        // Click without drag
        if (!tracks.empty()) {
            selectionController()->setSelectedTracks(tracks);
        } else {
            selectionController()->resetSelectedTracks();
        }
        setSelection(x1, x1, true);
        return;
    }

    if (tracks.empty()) {
        selectionController()->resetSelectedTracks();
        setSelection(x1, x2, true);
        return;
    }

    setSelectionActive(true);

    if (m_startPoint.y() < y) {
        selectionController()->setFocusedTrack(tracks.back());
    } else {
        selectionController()->setFocusedTrack(tracks.front());
    }
    selectionController()->setSelectedTracks(tracks, true);

    // time
    setSelection(x1, x2, true);
    
    // spectral selection
    double y1 = m_startPoint.y();
    double y2 = y;
    setSpectralSelection(y1, y2, true);
}

void SelectionViewController::onSelectionDraged(double x1, double x2, bool completed)
{
    if (!isProjectOpened()) {
        return;
    }

    // time
    if (x1 > x2) {
        std::swap(x1, x2);
    }

    setSelection(x1, x2, completed);
    m_selectionEditInProgress = !completed;
    emit selectionEditInProgressChanged();
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

    const std::vector<TrackId> tracks = vs->tracksInRange(m_startPoint.y(), y);
    if (tracks.empty()) {
        return;
    }

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
}

bool SelectionViewController::isLeftSelection(double x) const
{
    if (!isProjectOpened()) {
        return false;
    }

    return m_startPoint.x() > x;
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

bool SelectionViewController::selectionActive() const
{
    return m_selectionActive;
}

bool SelectionViewController::selectionEditInProgress() const
{
    return m_selectionEditInProgress;
}

bool SelectionViewController::selectionInProgress() const
{
    return m_selectionStarted;
}

void SelectionViewController::setSelectionActive(bool newSelectionActive)
{
    if (m_selectionActive == newSelectionActive) {
        return;
    }
    m_selectionActive = newSelectionActive;
    emit selectionActiveChanged();
}

void SelectionViewController::setSelection(double x1, double x2, bool complete)
{
    selectionController()->setDataSelectedStartTime(m_context->positionToTime(x1, true /*withSnap*/), complete);
    selectionController()->setDataSelectedEndTime(m_context->positionToTime(x2, true /*withSnap*/), complete);
}

void SelectionViewController::setSpectralSelection(double y1, double y2, bool complete)
{
    if (!isSpectralSelectionEnabled()) {
        return;
    }

    // Get the track at this Y position
    IProjectViewStatePtr vs = viewState();
    if (!vs) {
        return;
    }

    trackedit::TrackId trackId = vs->trackAtPosition(m_startPoint.y());
    if (trackId == trackedit::TrackId(-1)) {
        return;
    }

    // Check if this track is in spectrogram view mode
    if (!isTrackInSpectrogramView(trackId)) {
        selectionController()->resetSpectralSelection();
        return;
    }

    // Convert Y coordinates to frequencies
    double f1 = yToFrequency(y1, trackId);
    double f2 = yToFrequency(y2, trackId);

    // Ensure f1 <= f2
    if (f1 > f2) {
        std::swap(f1, f2);
    }

    selectionController()->setSpectralSelectionStartFrequency(f1, complete);
    selectionController()->setSpectralSelectionEndFrequency(f2, complete);
}

double SelectionViewController::yToFrequency(double y, const trackedit::TrackId& trackId) const
{
    // TODO: This needs to be implemented properly to convert Y coordinates to frequency
    // For now, return a placeholder that will need to be updated with actual conversion logic
    // that takes into account the spectrogram scale type, min/max frequency, and track height
    
    IProjectViewStatePtr vs = viewState();
    if (!vs) {
        return -1.0;
    }

    int trackVertPos = vs->trackVerticalPosition(trackId);
    int trackHeight = vs->trackHeight(trackId).val;
    
    // Calculate relative position within track (0.0 at top, 1.0 at bottom)
    double relativeY = (y - trackVertPos) / static_cast<double>(trackHeight);
    
    // Clamp to [0, 1]
    relativeY = std::max(0.0, std::min(1.0, relativeY));
    
    // For now, use a simple linear mapping to frequency range
    // In reality, this should use the spectrogram's scale (linear, log, mel, etc.)
    // and the track's min/max frequency settings
    constexpr double minFreq = 0.0;
    constexpr double maxFreq = 20000.0;
    
    // Invert Y (top = high frequency, bottom = low frequency)
    double frequency = maxFreq - (relativeY * (maxFreq - minFreq));
    
    return frequency;
}

bool SelectionViewController::isSpectralSelectionEnabled() const
{
    return spectrogramConfiguration()->spectralSelectionEnabled();
}

bool SelectionViewController::isTrackInSpectrogramView(const trackedit::TrackId& trackId) const
{
    auto prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return false;
    }

    auto track = prj->track(trackId);
    if (!track.has_value()) {
        return false;
    }

    return track->viewType == trackedit::TrackViewType::Spectrogram
           || track->viewType == trackedit::TrackViewType::WaveformAndSpectrogram;
}
