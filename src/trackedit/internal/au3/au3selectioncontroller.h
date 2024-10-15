/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../../iselectioncontroller.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "au3wrap/au3types.h"

namespace au::trackedit {
class Au3SelectionController : public ISelectionController
{
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    Au3SelectionController() = default;

    // track selection
    void resetSelectedTracks() override;
    trackedit::TrackIdList selectedTracks() const override;
    void setSelectedTracks(const trackedit::TrackIdList &trackIds, bool complete = true) override;
    muse::async::Channel<trackedit::TrackIdList> tracksSelected() const override;

    // clip selection
    void resetSelectedClip() override;
    trackedit::ClipKey selectedClip() const override;
    void setSelectedClip(const trackedit::ClipKey& clipKey) override;
    muse::async::Channel<trackedit::ClipKey> clipSelected() const override;

    // data selection
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

private:
    au3::Au3Project& projectRef() const;

    template<typename T>
    struct Val {
        T val = T();
        muse::async::Channel<T> changed;
        muse::async::Channel<T> selected;

        void set(const T& v, bool complete)
        {
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
    Val<trackedit::ClipKey> m_selectedClip;

    // data selection
    Val<trackedit::secs_t> m_selectedStartTime;
    Val<trackedit::secs_t> m_selectedEndTime;
};
}
