/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"

#include "au3-track/Track.h"

#include "au3wrap/au3types.h"
#include "context/iglobalcontext.h"

#include "trackedittypes.h"
#include "../../iselectioncontroller.h"
#include "../../iprojecthistory.h"

namespace au::trackedit {
class Au3SelectionController : public ISelectionController, public muse::async::Asyncable, public muse::Injectable
{
    muse::Inject<au::context::IGlobalContext> globalContext { this };
    muse::Inject<IProjectHistory> projectHistory { this };

public:
    Au3SelectionController(const muse::modularity::ContextPtr& ctx)
        : muse::Injectable(ctx) {}

    void init();

    // track selection
    void resetSelectedTracks() override;
    trackedit::TrackIdList selectedTracks() const override;
    void setSelectedTracks(const trackedit::TrackIdList& trackIds, bool complete) override;
    muse::async::Channel<TrackIdList> selectedTracksChanged() const override;
    muse::async::Channel<trackedit::TrackIdList> tracksSelected() const override;

    // clip selection
    void resetSelectedClips() override;
    bool hasSelectedClips() const override;
    ClipKeyList selectedClips() const override;
    ClipKeyList selectedClipsInTrackOrder() const override;
    void setSelectedClips(const ClipKeyList& clipKeys, bool complete) override;
    void addSelectedClip(const ClipKey& clipKey) override;
    void removeClipSelection(const ClipKey& clipKey) override;
    muse::async::Channel<ClipKeyList> clipsSelected() const override;
    std::optional<secs_t> selectedClipStartTime() const override;
    std::optional<secs_t> selectedClipEndTime() const override;
    std::optional<secs_t> leftMostSelectedClipStartTime() const override;
    std::optional<secs_t> rightMostSelectedClipEndTime() const override;

    // label selection
    void resetSelectedLabels() override;
    bool hasSelectedLabels() const override;
    LabelKeyList selectedLabels() const override;
    LabelKeyList selectedLabelsInTrackOrder() const override;
    void setSelectedLabels(const LabelKeyList& labelKeys, bool complete) override;
    void addSelectedLabel(const LabelKey& labelKey) override;
    void removeLabelSelection(const LabelKey& labelKey) override;
    muse::async::Channel<LabelKeyList> labelsSelected() const override;
    std::optional<secs_t> selectedLabelStartTime() const override;
    std::optional<secs_t> selectedLabelEndTime() const override;
    std::optional<secs_t> leftMostSelectedLabelStartTime() const override;
    std::optional<secs_t> rightMostSelectedLabelEndTime() const override;

    std::optional<secs_t> leftMostSelectedItemStartTime() const override;
    std::optional<secs_t> rightMostSelectedItemEndTime() const override;

    std::optional<secs_t> selectedTracksStartTime() const override;
    std::optional<secs_t> selectedTracksEndTime() const override;

    // data selection
    void setSelectedTrackAudioData(trackedit::TrackId trackId) override;
    void resetDataSelection() override;
    bool timeSelectionIsNotEmpty() const override;
    bool timeSelectionHasAudioData() const override;
    bool isDataSelectedOnTrack(TrackId trackId) const override;
    void setSelectedAllAudioData(const std::optional<secs_t>& fromTime = std::nullopt,
                                 const std::optional<secs_t>& toTime = std::nullopt) override;
    ClipKeyList clipsIntersectingRangeSelection() const override;
    void setClipsIntersectingRangeSelection(const ClipKeyList& clipKeys) override;
    muse::async::Channel<ClipKeyList> clipsIntersectingRangeSelectionChanged() const override;
    LabelKeyList labelsIntersectingRangeSelection() const override;
    void setLabelsIntersectingRangeSelection(const LabelKeyList& labelKeys) override;

    trackedit::secs_t dataSelectedStartTime() const override;
    void setDataSelectedStartTime(trackedit::secs_t time, bool complete) override;
    muse::async::Channel<trackedit::secs_t> dataSelectedStartTimeChanged() const override;
    muse::async::Channel<trackedit::secs_t> dataSelectedStartTimeSelected() const override;

    trackedit::secs_t dataSelectedEndTime() const override;
    void setDataSelectedEndTime(trackedit::secs_t time, bool complete) override;
    muse::async::Channel<trackedit::secs_t> dataSelectedEndTimeChanged() const override;
    muse::async::Channel<trackedit::secs_t> dataSelectedEndTimeSelected() const override;

    trackedit::secs_t selectionStartTime() const override;
    void setSelectionStartTime(trackedit::secs_t time) override;

    std::pair<double, double> frequencySelection(trackedit::TrackId trackId) const override;
    void setFrequencySelection(trackedit::TrackId, const std::pair<double, double>& selection) override;
    void resetFrequencySelection() override;
    muse::async::Channel<trackedit::TrackId> frequencySelectionChanged() const override;

    // grouping
    bool selectionContainsGroup() const override;
    bool isSelectionGrouped() const override;

    void resetTimeSelection() override;

    trackedit::TrackId focusedTrack() const override;
    void setFocusedTrack(TrackId trackId) override;
    muse::async::Channel<trackedit::TrackId> focusedTrackChanged() const override;

    void focusPreviousTrack() override;
    void focusNextTrack() override;
    void focusTrackByIndex(int index) override;

    int trackDistance(const TrackId previous, const TrackId next) const override;
    TrackIdList orderedTrackList() const override;

private:
    void addSelectedTrack(const trackedit::TrackId& trackId);
    void updateSelectionController();
    void onUndoRedo();
    ClipKeyList findClipsIntersectingRangeSelection() const;
    LabelKeyList findLabelsIntersectingRangeSelection() const;

    au3::Au3Project& projectRef() const;
    Observer::Subscription m_tracksSubc;

    template<typename T>
    struct Val {
        Val()
            : val(T()) {}
        Val(const T& val)
            : val(val) {}
        T val;
        muse::async::Channel<T> changed;
        muse::async::Channel<T> selected;

        void set(const T& v, bool complete)
        {
            if (val == v) {
                return;
            }
            val = v;
            changed.send(v);
            if (complete) {
                selected.send(v);
            }
        }
    };

    // track selection
    Val<TrackIdList> m_selectedTracks;

    // clip selection
    Val<ClipKeyList> m_selectedClips;

    // label selection
    Val<LabelKeyList> m_selectedLabels;

    // data selection
    Val<trackedit::secs_t> m_selectedStartTime;
    Val<trackedit::secs_t> m_selectedEndTime;
    Val<ClipKeyList> m_clipsIntersectingRangeSelection;
    Val<LabelKeyList> m_labelsIntersectingRangeSelection;

    Val<trackedit::secs_t> m_selectionStartTime; // indicates where user started selection

    // track focus state
    Val<TrackId> m_focusedTrack = Val<TrackId> { TrackId(-1) };

    struct TrackFrequencySelection {
        const int trackId;
        const double startFrequency;
        const double endFrequency;

        constexpr bool operator==(const TrackFrequencySelection& other) const
        {
            return trackId == other.trackId
                   && startFrequency == other.startFrequency
                   && endFrequency == other.endFrequency;
        }

        constexpr bool operator!=(const TrackFrequencySelection& other) const
        {
            return !(*this == other);
        }
    };

    std::optional<TrackFrequencySelection> m_frequencySelection;
    muse::async::Channel<trackedit::TrackId> m_frequencySelectionChanged;
};
}
