/*
* Audacity: A Digital Audio Editor
*/
#include "trackitemsmovecontroller.h"

#include <algorithm>
#include <QScopedValueRollback>

#include "global/realfn.h"

using namespace au::projectscene;
using namespace au::trackedit;

namespace {
int indexOf(const TrackIdList& tracks, TrackId id)
{
    const auto it = std::find(tracks.begin(), tracks.end(), id);
    return it == tracks.end() ? -1 : static_cast<int>(std::distance(tracks.begin(), it));
}

TrackIdList tracksOfKind(const std::vector<Track>& tracks, bool labels)
{
    TrackIdList ids;
    for (const Track& track : tracks) {
        if (labels ? track.type == TrackType::Label : track.type == TrackType::Mono || track.type == TrackType::Stereo) {
            ids.push_back(track.id);
        }
    }
    return ids;
}
}

TrackItemsMoveController::TrackItemsMoveController(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

TrackItemsMoveController::~TrackItemsMoveController()
{
    if (active() && globalContext()->currentTrackeditProject() == m_project) {
        cancel();
    }
}

TimelineContext* TrackItemsMoveController::timelineContext() const
{
    return m_context;
}

void TrackItemsMoveController::setTimelineContext(TimelineContext* context)
{
    if (m_context == context) {
        return;
    }
    if (m_context) {
        disconnect(m_context, nullptr, this, nullptr);
    }
    m_context = context;
    if (m_context) {
        connect(m_context, &TimelineContext::frameTimeChanged, this, [this] {
            if (m_moved) {
                update();
            }
        });
    }
    emit contextChanged();
}

void TrackItemsMoveController::start(const TrackItemKey& key)
{
    if (active() || !m_context || !key.isValid()) {
        return;
    }
    const auto project = globalContext()->currentProject();
    const auto trackeditProject = globalContext()->currentTrackeditProject();
    if (!project || !project->viewState() || !trackeditProject) {
        return;
    }
    const auto track = trackeditProject->track(key.key.trackId);
    if (!track) {
        return;
    }
    m_sourceIsLabel = track->type == TrackType::Label;
    if (m_sourceIsLabel) {
        const Label label = trackeditProject->label(key.key);
        if (!label.isValid()) {
            return;
        }
        m_startTime = label.startTime;
        m_endTime = label.endTime;
    } else {
        const Clip clip = trackeditProject->clip(key.key);
        if (!clip.isValid()) {
            return;
        }
        m_startTime = clip.startTime;
        m_endTime = clip.endTime;
    }

    m_project = trackeditProject;
    m_viewState = project->viewState();
    m_sourceKey = key.key;
    m_clips = selectionController()->selectedClipsInTrackOrder();
    m_labels = selectionController()->selectedLabels();
    m_rangeSelection = !selectionController()->timeSelectionIsEmpty();
    m_originalTrackCount = m_project->trackList().size();

    projectHistory()->startUserInteraction();
    m_viewState->setItemEditStartTimeOffset(m_context->mousePositionTime() - m_startTime);
    m_viewState->setItemEditEndTimeOffset(m_endTime - m_context->mousePositionTime());
    m_viewState->setEditedItem(m_sourceKey);
    m_viewState->updateItemsBoundaries(true, m_sourceKey);
    emit activeChanged();
}

bool TrackItemsMoveController::active() const
{
    return m_sourceKey.isValid();
}

bool TrackItemsMoveController::isDragged(const trackedit::TrackItemKey& key) const
{
    return m_moved && !m_rangeSelection && (muse::contains(m_clips, key) || muse::contains(m_labels, key));
}

double TrackItemsMoveController::timeOffset() const
{
    return m_timeOffset;
}

double TrackItemsMoveController::pointerTimeOffset(double start, double end) const
{
    double newStart = m_context->mousePositionTime() - m_viewState->itemEditStartTimeOffset();
    const double duration = end - start;
    const double newEnd = newStart + duration;
    const double snappedStart = m_viewState->isSnapEnabled()
                                ? m_context->applySnapToTime(newStart) : m_context->applySnapToItem(newStart);
    const double snappedEnd = m_viewState->isSnapEnabled() ? newEnd : m_context->applySnapToItem(newEnd);
    if (muse::RealIsEqual(snappedEnd, newEnd)) {
        newStart = snappedStart;
    } else if (muse::RealIsEqual(snappedStart, newStart)) {
        newStart = snappedEnd - duration;
    } else {
        newStart = !muse::RealIsEqualOrMore(std::abs(snappedStart - newStart), std::abs(snappedEnd - newEnd))
                   ? snappedStart : snappedEnd - duration;
    }
    const double offset = newStart - start;
    return muse::RealIsEqualOrMore(std::abs(offset), 1.0 / 192000.0) ? offset : 0.0;
}

int TrackItemsMoveController::pointerTrackOffset() const
{
    const TrackIdList tracks = tracksOfKind(m_project->trackList(), m_sourceIsLabel);
    const int source = indexOf(tracks, m_sourceKey.trackId);
    if (source < 0) {
        return m_trackOffset;
    }
    const double y = m_viewState->mousePositionY();
    const int target = indexOf(tracks, m_viewState->trackAtPosition(y));
    if (target >= 0) {
        return target - source;
    }
    const double bottom = m_viewState->totalTrackHeight().val - m_viewState->tracksVerticalOffset().val;
    if (y > bottom) {
        return static_cast<int>(tracks.size()) - source
               + static_cast<int>((y - bottom) / m_viewState->trackDefaultHeight());
    }
    // Keep the last destination while the pointer crosses another kind of track.
    return m_trackOffset;
}

void TrackItemsMoveController::update()
{
    if (!active() || !m_context || m_updating || globalContext()->currentTrackeditProject() != m_project) {
        return;
    }
    QScopedValueRollback<bool> guard(m_updating, true);

    double start = m_startTime;
    double end = m_endTime;
    if (m_rangeSelection) {
        // Range moves still edit incrementally; their source times change after each update.
        if (m_sourceIsLabel) {
            const Label label = m_project->label(m_sourceKey);
            start = label.startTime;
            end = label.endTime;
        } else {
            const Clip clip = m_project->clip(m_sourceKey);
            start = clip.startTime;
            end = clip.endTime;
        }
    }
    const double offset = pointerTimeOffset(start, end);
    const int trackOffset = pointerTrackOffset();
    if (!m_moved && std::abs(offset * m_context->zoom()) < 3.0 && trackOffset == 0) {
        return;
    }
    m_moved = true;
    m_viewState->setMoveInitiated(true);

    if (m_rangeSelection) {
        trackeditInteraction()->moveRangeSelection(offset, false);
    } else {
        updatePreview(offset, trackOffset);
    }
    const double appliedOffset = m_rangeSelection ? offset : m_timeOffset;
    double guideline = m_context->findGuideline(start + appliedOffset);
    if (!m_context->isGuidelineValid(guideline)) {
        guideline = m_context->findGuideline(end + appliedOffset);
    }
    emit guidelineChanged(guideline);
}

void TrackItemsMoveController::updatePreview(double timeOffset, int trackOffset)
{
    for (const trackedit::ClipKey& key : m_clips) {
        timeOffset = std::max(timeOffset, -m_project->clip(key).startTime);
    }
    for (const trackedit::LabelKey& key : m_labels) {
        timeOffset = std::max(timeOffset, -m_project->label(key).startTime);
    }

    const auto tracks = m_project->trackList();
    const TrackIdList audio = tracksOfKind(tracks, false);
    const int originalAudioCount = static_cast<int>(audio.size()) - static_cast<int>(tracks.size() - m_originalTrackCount);
    for (const trackedit::ClipKey& key : m_clips) {
        const int source = indexOf(audio, key.trackId);
        if (source >= 0) {
            trackOffset = std::max(trackOffset, -source);
        }
    }
    int extraTracks = 0;
    for (const trackedit::ClipKey& key : m_clips) {
        const int source = indexOf(audio, key.trackId);
        if (source >= 0) {
            extraTracks = std::max(extraTracks, source + trackOffset - (originalAudioCount - 1));
        }
    }
    const size_t targetCount = m_originalTrackCount + extraTracks;
    if (tracks.size() > targetCount) {
        tracksInteraction()->removeDragAddedTracks(targetCount, true /* emptyOnly */);
    }
    for (size_t i = tracks.size(); i < targetCount; ++i) {
        tracksInteraction()->addWaveTrack(1);
    }
    m_timeOffset = timeOffset;
    m_trackOffset = trackOffset;
    emit previewChanged();
}

TrackItemKeyList TrackItemsMoveController::itemsOnTrack(TrackId trackId) const
{
    TrackItemKeyList keys;
    if (!m_moved || m_rangeSelection) {
        return keys;
    }
    const auto tracks = m_project->trackList();
    const bool labels = muse::contains(tracksOfKind(tracks, true), trackId);
    const TrackIdList destinations = tracksOfKind(tracks, labels);
    const TrackItemKeyList& items = labels ? m_labels : m_clips;
    for (const trackedit::TrackItemKey& key : items) {
        const int source = indexOf(destinations, key.trackId);
        if (source < 0) {
            continue;
        }
        const int target = labels ? std::clamp(source + m_trackOffset, 0, static_cast<int>(destinations.size()) - 1)
                           : source + m_trackOffset;
        if (target >= 0 && target < static_cast<int>(destinations.size()) && destinations[target] == trackId) {
            keys.push_back(key);
        }
    }
    return keys;
}

au::projectscene::TrackItemKey TrackItemsMoveController::finish()
{
    if (!active() || m_updating) {
        return {};
    }
    TrackItemKey movedKey(m_sourceKey);
    if (m_moved) {
        update();
        QScopedValueRollback<bool> guard(m_updating, true);
        if (m_rangeSelection) {
            trackeditInteraction()->moveRangeSelection(0.0, true);
        } else {
            m_moved = false;
            emit previewChanged();

            const TrackItemKeyList& selected = m_sourceIsLabel ? m_labels : m_clips;
            bool changedTrack = false;
            const auto result = m_sourceIsLabel
                                ? trackeditInteraction()->moveLabels(selected, m_timeOffset, m_trackOffset, true)
                                : trackeditInteraction()->moveClips(selected, m_timeOffset, m_trackOffset, true, changedTrack);
            if (result.ret) {
                const auto source = std::find(selected.begin(), selected.end(), m_sourceKey);
                const auto sourceIndex = std::distance(selected.begin(), source);
                if (source != selected.end() && sourceIndex < static_cast<int>(result.val.size())) {
                    movedKey = TrackItemKey(result.val[sourceIndex]);
                }
                if (m_sourceIsLabel) {
                    selectionController()->setSelectedLabels(result.val, true);
                } else {
                    selectionController()->setSelectedClips(result.val, true);
                }
            }
        }
    }
    endInteraction();
    return movedKey;
}

bool TrackItemsMoveController::cancel()
{
    if (!active() || m_updating) {
        return false;
    }
    QScopedValueRollback<bool> guard(m_updating, true);
    if (m_moved) {
        if (m_rangeSelection) {
            trackeditInteraction()->cancelItemDragEdit();
        } else {
            tracksInteraction()->removeDragAddedTracks(m_originalTrackCount, true /* emptyOnly */);
        }
    }
    endInteraction();
    return true;
}

void TrackItemsMoveController::endInteraction()
{
    if (m_context) {
        m_context->stopAutoScroll();
    }
    m_moved = false;
    m_timeOffset = 0.0;
    m_trackOffset = 0;
    m_sourceKey = {};
    m_clips.clear();
    m_labels.clear();
    m_viewState->setMoveInitiated(false);
    m_viewState->setItemEditStartTimeOffset(-1.0);
    m_viewState->setItemEditEndTimeOffset(-1.0);
    m_viewState->setEditedItem({});
    m_viewState->updateItemsBoundaries(false);
    emit previewChanged();
    emit guidelineChanged(TimelineContext::INVALID_GUIDELINE_TIME);
    emit activeChanged();
    projectHistory()->endUserInteraction();
    m_viewState.reset();
    m_project.reset();
}
