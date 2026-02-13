/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "framework/global/modularity/imoduleinterface.h"
#include "framework/global/async/channel.h"

#include "trackedit/trackedittypes.h"

#include <optional>

namespace au::trackedit {
//! NOTE Currently implemented in the au3wrap module
class ISelectionController : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(trackedit::ISelectionController)

public:
    virtual ~ISelectionController() = default;

    //! NOTE typical behavior
    //! * getter - getting the currently selected value (the selection itself may not be complete yet)
    //! * setter - setting the selected values ​​during selection,
    //! if the selection is completed, then it is necessary to pass: `complete = true`
    //! * valueChanged - notified when the selected value change (for example, the user moves the mouse cursor)
    //! * valueSelected - notified when value selection is complete

    // track selection
    virtual void resetSelectedTracks() = 0;
    virtual TrackIdList selectedTracks() const = 0;
    virtual void setSelectedTracks(const TrackIdList& trackIds, bool complete = true) = 0;
    virtual muse::async::Channel<TrackIdList> selectedTracksChanged() const = 0;
    virtual muse::async::Channel<TrackIdList> tracksSelected() const = 0;

    // clip selection
    virtual void resetSelectedClips() = 0;
    virtual bool hasSelectedClips() const = 0;
    virtual ClipKeyList selectedClips() const = 0;
    virtual ClipKeyList selectedClipsInTrackOrder() const = 0;
    virtual void setSelectedClips(const ClipKeyList& clipKeys, bool complete = true) = 0;
    virtual void addSelectedClip(const ClipKey& clipKey) = 0;
    virtual void removeClipSelection(const ClipKey& clipKey) = 0;
    virtual muse::async::Channel<ClipKeyList> clipsSelected() const = 0;

    virtual std::optional<secs_t> selectedClipStartTime() const = 0;
    virtual std::optional<secs_t> selectedClipEndTime() const = 0;

    // used to get range-like boundaries of multiple selected clips
    virtual std::optional<secs_t> leftMostSelectedClipStartTime() const = 0;
    virtual std::optional<secs_t> rightMostSelectedClipEndTime() const = 0;

    // label selection
    virtual void resetSelectedLabels() = 0;
    virtual bool hasSelectedLabels() const = 0;
    virtual LabelKeyList selectedLabels() const = 0;
    virtual LabelKeyList selectedLabelsInTrackOrder() const = 0;
    virtual void setSelectedLabels(const LabelKeyList& labelKeys, bool complete = true) = 0;
    virtual void addSelectedLabel(const LabelKey& labelKey) = 0;
    virtual void removeLabelSelection(const LabelKey& labelKey) = 0;
    virtual muse::async::Channel<LabelKeyList> labelsSelected() const = 0;

    virtual std::optional<secs_t> selectedLabelStartTime() const = 0;
    virtual std::optional<secs_t> selectedLabelEndTime() const = 0;

    virtual std::optional<secs_t> leftMostSelectedLabelStartTime() const = 0;
    virtual std::optional<secs_t> rightMostSelectedLabelEndTime() const = 0;

    virtual std::optional<secs_t> leftMostSelectedItemStartTime() const = 0;
    virtual std::optional<secs_t> rightMostSelectedItemEndTime() const = 0;

    virtual std::optional<secs_t> selectedTracksStartTime() const = 0;
    virtual std::optional<secs_t> selectedTracksEndTime() const = 0;

    // data selection
    virtual void setSelectedTrackAudioData(trackedit::TrackId trackId) = 0;
    virtual void resetDataSelection() = 0;
    virtual bool timeSelectionIsNotEmpty() const = 0;
    virtual bool timeSelectionHasAudioData() const = 0;
    virtual bool isDataSelectedOnTrack(TrackId trackId) const = 0;
    virtual void setSelectedAllAudioData(const std::optional<secs_t>& fromTime = std::nullopt,
                                         const std::optional<secs_t>& toTime = std::nullopt) = 0;
    virtual ClipKeyList clipsIntersectingRangeSelection() const = 0;
    virtual void setClipsIntersectingRangeSelection(const ClipKeyList& clipKeys) = 0;
    virtual muse::async::Channel<ClipKeyList> clipsIntersectingRangeSelectionChanged() const = 0;
    virtual LabelKeyList labelsIntersectingRangeSelection() const = 0;
    virtual void setLabelsIntersectingRangeSelection(const LabelKeyList& labelKeys) = 0;

    virtual secs_t dataSelectedStartTime() const = 0;
    virtual void setDataSelectedStartTime(secs_t time, bool complete) = 0;
    virtual muse::async::Channel<secs_t> dataSelectedStartTimeChanged() const = 0;
    virtual muse::async::Channel<secs_t> dataSelectedStartTimeSelected() const = 0;

    virtual secs_t dataSelectedEndTime() const = 0;
    virtual void setDataSelectedEndTime(secs_t time, bool complete) = 0;
    virtual muse::async::Channel<secs_t> dataSelectedEndTimeChanged() const = 0;
    virtual muse::async::Channel<secs_t> dataSelectedEndTimeSelected() const = 0;

    virtual std::pair<double, double> frequencySelection(trackedit::TrackId trackId = -1) const = 0;
    virtual void setFrequencySelection(trackedit::TrackId, const std::pair<double, double>& selection) = 0;
    virtual void resetFrequencySelection() = 0;
    virtual muse::async::Channel<trackedit::TrackId> frequencySelectionChanged() const = 0;

    virtual trackedit::secs_t selectionStartTime() const = 0;
    virtual void setSelectionStartTime(trackedit::secs_t time) = 0;

    // grouping
    virtual bool selectionContainsGroup() const = 0;
    virtual bool isSelectionGrouped() const = 0;

    virtual void resetTimeSelection() = 0;

    virtual int trackDistance(const TrackId previous, const TrackId next) const = 0;
    virtual TrackIdList orderedTrackList() const = 0;
};
}
