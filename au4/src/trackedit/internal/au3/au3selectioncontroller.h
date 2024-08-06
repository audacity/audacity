/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../../iselectioncontroller.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

class AudacityProject;
namespace au::trackedit {
class Au3SelectionController : public ISelectionController
{
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    Au3SelectionController() = default;

    // track selection
    void resetSelectedTrack() override;
    trackedit::TrackId selectedTrack() const override;
    void setSelectedTrack(trackedit::TrackId trackId) override;
    muse::async::Channel<trackedit::TrackId> trackSelected() const override;

    // clip selection
    void resetSelectedClip() override;
    trackedit::ClipKey selectedClip() const override;
    void setSelectedClip(const trackedit::ClipKey& clipKey) override;
    muse::async::Channel<trackedit::ClipKey> clipSelected() const override;

    // data selection
    void resetDataSelection() override;
    bool isDataSelected() override;

    std::vector<trackedit::TrackId> dataSelectedOnTracks() const override;
    void setDataSelectedOnTracks(const std::vector<trackedit::TrackId>& trackIds, bool complete) override;
    muse::async::Channel<std::vector<trackedit::TrackId>> dataSelectedOnTracksChanged() const override;
    muse::async::Channel<std::vector<trackedit::TrackId>> dataSelectedOnTracksSelected() const override;

    trackedit::secs_t dataSelectedStartTime() const override;
    void setDataSelectedStartTime(trackedit::secs_t time, bool complete) override;
    muse::async::Channel<trackedit::secs_t> dataSelectedStartTimeChanged() const override;
    muse::async::Channel<trackedit::secs_t> dataSelectedStartTimeSelected() const override;

    trackedit::secs_t dataSelectedEndTime() const override;
    void setDataSelectedEndTime(trackedit::secs_t time, bool complete) override;
    muse::async::Channel<trackedit::secs_t> dataSelectedEndTimeChanged() const override;
    muse::async::Channel<trackedit::secs_t> dataSelectedEndTimeSelected() const override;

private:
    AudacityProject& projectRef() const;

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
    Val<trackedit::TrackId> m_selectedTrack;

    // clip selection
    Val<trackedit::ClipKey> m_selectedClip;

    // data selection
    Val<std::vector<trackedit::TrackId>> m_selectedTrackIds;
    Val<trackedit::secs_t> m_selectedStartTime;
    Val<trackedit::secs_t> m_selectedEndTime;
};
}
