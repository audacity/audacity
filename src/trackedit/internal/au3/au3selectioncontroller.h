/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../../iselectioncontroller.h"

#include "async/asyncable.h"
#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "playback/iplayback.h"

#include "Track.h"

#include "au3wrap/au3types.h"

namespace au::trackedit {
class Au3SelectionController : public ISelectionController, public muse::async::Asyncable
{
    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<au::playback::IPlayback> playback;

public:
    Au3SelectionController() = default;

    void init();

    // track selection
    void resetSelectedTracks() override;
    trackedit::TrackIdList selectedTracks() const override;
    void setSelectedTracks(const trackedit::TrackIdList& trackIds, bool complete = true) override;
    void addSelectedTrack(const trackedit::TrackId& trackId) override;
    muse::async::Channel<trackedit::TrackIdList> tracksSelected() const override;
    std::optional<TrackId> determinePointedTrack(double y) const override;
    trackedit::TrackIdList determinateTracks(double y1, double y2) const override;

    // clip selection
    void resetSelectedClips() override;
    bool hasSelectedClips() const override;
    ClipKeyList selectedClips() const override;
    ClipKeyList selectedClipsInTrackOrder() const override;
    void setSelectedClips(const ClipKeyList& clipKeys, bool complete = true) override;
    std::optional<trackedit::ClipId> setSelectedClip(trackedit::TrackId trackId, secs_t time) override;
    void addSelectedClip(const ClipKey& clipKey) override;
    void removeClipSelection(const ClipKey& clipKey) override;
    muse::async::Channel<ClipKeyList> clipsSelected() const override;
    double selectedClipStartTime() const override;
    double selectedClipEndTime() const override;

    // data selection
    void setSelectedTrackAudioData(trackedit::TrackId trackId) override;
    void setSelectedClipAudioData(trackedit::TrackId trackId, secs_t time) override;
    void resetDataSelection() override;
    bool timeSelectionIsNotEmpty() const override;
    bool isDataSelectedOnTrack(TrackId trackId) const override;

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

private:
    void updateSelectionController();

    au3::Au3Project& projectRef() const;
    Observer::Subscription m_tracksSubc;

    template<typename T>
    struct Val {
        T val = T();
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

    // data selection
    Val<trackedit::secs_t> m_selectedStartTime;
    Val<trackedit::secs_t> m_selectedEndTime;

    Val<trackedit::secs_t> m_selectionStartTime; // indicates where user started selection
};
}
