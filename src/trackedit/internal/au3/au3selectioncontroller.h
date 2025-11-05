/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"

#include "libraries/lib-track/Track.h"

#include "au3wrap/au3types.h"
#include "context/iglobalcontext.h"

#include "trackedittypes.h"
#include "../../iselectioncontroller.h"

namespace au::trackedit {
struct ClipAndTimeSelection;

class Au3SelectionController : public ISelectionController, public muse::async::Asyncable
{
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    Au3SelectionController() = default;

    void init();

    // track selection
    void resetSelectedTracks() override;
    trackedit::TrackIdList selectedTracks() const override;
    void setSelectedTracks(const trackedit::TrackIdList& trackIds, bool complete) override;
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
    double selectedClipStartTime() const override;
    double selectedClipEndTime() const override;
    double leftMostSelectedClipStartTime() const override;
    double rightMostSelectedClipEndTime() const override;

    // label selection
    void resetSelectedLabels() override;
    bool hasSelectedLabels() const override;
    LabelKeyList selectedLabels() const override;
    LabelKeyList selectedLabelsInTrackOrder() const override;
    void setSelectedLabels(const LabelKeyList& labelKeys, bool complete) override;
    void addSelectedLabel(const LabelKey& labelKey) override;
    void removeLabelSelection(const LabelKey& labelKey) override;
    muse::async::Channel<LabelKeyList> labelsSelected() const override;
    double selectedLabelStartTime() const override;
    double selectedLabelEndTime() const override;
    double leftMostSelectedLabelStartTime() const override;
    double rightMostSelectedLabelEndTime() const override;

    // data selection
    void setSelectedTrackAudioData(trackedit::TrackId trackId) override;
    void resetDataSelection() override;
    bool timeSelectionIsNotEmpty() const override;
    bool timeSelectionHasAudioData() const override;
    bool isDataSelectedOnTrack(TrackId trackId) const override;
    void setSelectedAllAudioData() override;
    ClipKeyList clipsIntersectingRangeSelection() const override;
    void setClipsIntersectingRangeSelection(const ClipKeyList& clipKeys) override;
    ClipKeyList findClipsIntersectingRangeSelection() const override;

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
    void restoreSelection(const ClipAndTimeSelection& selection);

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

    Val<trackedit::secs_t> m_selectionStartTime; // indicates where user started selection

    // track focus state
    Val<TrackId> m_focusedTrack = Val<TrackId> { TrackId(-1) };
};
}
