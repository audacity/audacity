/*
* Audacity: A Digital Audio Editor
*/
#include <QApplication>

#include "selectionviewcontroller.h"

#include "spectrogram/view/spectrogramhit.h"
#include "framework/global/log.h"

using namespace au::projectscene;
using namespace au::project;
using namespace au::trackedit;

//! NOTE: sync with ItemsSelection.qml minSelection
constexpr double MIN_SELECTION_PX = 1.0;

SelectionViewController::SelectionViewController(QObject* parent)
    : QObject(parent), muse::Injectable(muse::iocCtxForQmlObject(this))
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

    spectrogramConfiguration()->spectralSelectionEnabledChanged().onReceive(this, [this] (auto) {
        emit spectralSelectionEnabledChanged();
    });
}

void SelectionViewController::onPressed(double x, double y, const spectrogram::SpectrogramHit* spectrogramHit)
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

    m_spectrogramHit = nullptr;
    if (spectralSelectionEnabled() && spectrogramHit) {
        m_spectrogramHit = spectrogramHit;
    }
    emit pressedSpectrogramChanged();
    setFrequencySelection(y, y);

    viewState()->updateItemsBoundaries(true);

    m_autoScrollConnection = connect(m_context, &TimelineContext::frameTimeChanged, [this]() {
        doOnPositionChanged(m_lastPoint.x(), m_lastPoint.y());
    });
}

void SelectionViewController::onPositionChanged(double x, double y)
{
    if (m_spectrogramHit && isInExtendedSpectrogram(*m_spectrogramHit, y)) {
        y = std::clamp(y, m_spectrogramHit->spectrogramY, m_spectrogramHit->spectrogramY + m_spectrogramHit->spectrogramHeight);
    }
    if (doOnPositionChanged(x, y)) {
        setFrequencySelection(y, m_startPoint.y());
    }
}

bool SelectionViewController::doOnPositionChanged(double x, double y)
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

    return true;
}

void SelectionViewController::onReleased(double x, double y)
{
    if (!isProjectOpened()) {
        return;
    }

    m_spectrogramHit = nullptr;

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
}

void SelectionViewController::onSelectionHorizontalResize(double x1, double x2, bool completed)
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

void SelectionViewController::startSelectionVerticalResize(const spectrogram::SpectrogramHit* hit)
{
    m_spectrogramHit = hit;
    m_selectionEditInProgress = true;
    emit selectionEditInProgressChanged();
}

void SelectionViewController::updateSelectionVerticalResize(double y1, double y2, bool completed)
{
    IF_ASSERT_FAILED(m_spectrogramHit) {
        return;
    }

    const auto vs = viewState();
    IF_ASSERT_FAILED(vs) {
        return;
    }

    const trackedit::TrackIdList tracks = vs->tracksInRange(y1, y2);
    selectionController()->setSelectedTracks(tracks, completed);

    setFrequencySelection(y1, y2);

    if (completed) {
        m_spectrogramHit = nullptr;
        m_selectionEditInProgress = false;
        emit selectionEditInProgressChanged();
    }
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

void SelectionViewController::setSelection(double x1, double x2, bool complete)
{
    selectionController()->setDataSelectedStartTime(m_context->positionToTime(x1, true /*withSnap*/), complete);
    selectionController()->setDataSelectedEndTime(m_context->positionToTime(x2, true /*withSnap*/), complete);
}

double SelectionViewController::spectrogramHitFrequency(const spectrogram::SpectrogramHit& hit, double y) const
{
    return spectrogramService()->yToFrequency(hit.trackId, y - hit.spectrogramY, hit.spectrogramHeight);
}

void SelectionViewController::setFrequencySelection(double y1, double y2)
{
    if (!m_spectrogramHit || !isInExtendedSpectrogram(*m_spectrogramHit, y1) || !isInExtendedSpectrogram(*m_spectrogramHit, y2)) {
        selectionController()->resetFrequencySelection();
        return;
    }

    auto freq1 = spectrogramHitFrequency(*m_spectrogramHit, y1);
    auto freq2 = spectrogramHitFrequency(*m_spectrogramHit, y2);
    if (freq1 > freq2) {
        std::swap(freq1, freq2);
    }
    selectionController()->setFrequencySelection(m_spectrogramHit->trackId, std::make_pair(freq1, freq2));
}

bool SelectionViewController::isInExtendedSpectrogram(const spectrogram::SpectrogramHit& hit, double y) const
{
    return hit.spectrogramY - m_resistancePx <= y && y <= hit.spectrogramY + hit.spectrogramHeight + m_resistancePx;
}
