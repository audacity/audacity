/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "processing/iselectioncontroller.h"

class AudacityProject;
namespace au::au3 {
class Au3SelectionController : public processing::ISelectionController
{
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    Au3SelectionController() = default;

    // track selection
    void resetSelectedTrack() override;
    processing::TrackId selectedTrack() const override;
    void setSelectedTrack(processing::TrackId trackId) override;
    muse::async::Channel<processing::TrackId> trackSelected() const override;

    // clip selection
    void resetSelectedClip() override;
    processing::ClipKey selectedClip() const override;
    void setSelectedClip(const processing::ClipKey& clipKey) override;
    muse::async::Channel<processing::ClipKey> clipSelected() const override;

    // data selection
    void resetDataSelection() override;

    std::vector<processing::TrackId> dataSelectedOnTracks() const override;
    void setDataSelectedOnTracks(const std::vector<processing::TrackId>& trackIds, bool complete) override;
    muse::async::Channel<std::vector<processing::TrackId>> dataSelectedOnTracksChanged() const override;
    muse::async::Channel<std::vector<processing::TrackId>> dataSelectedOnTracksSelected() const override;

    processing::secs_t dataSelectedStartTime() const override;
    void setDataSelectedStartTime(processing::secs_t time, bool complete) override;
    muse::async::Channel<processing::secs_t> dataSelectedStartTimeChanged() const override;
    muse::async::Channel<processing::secs_t> dataSelectedStartTimeSelected() const override;

    processing::secs_t dataSelectedEndTime() const override;
    void setDataSelectedEndTime(processing::secs_t time, bool complete) override;
    muse::async::Channel<processing::secs_t> dataSelectedEndTimeChanged() const override;
    muse::async::Channel<processing::secs_t> dataSelectedEndTimeSelected() const override;

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
    Val<processing::TrackId> m_selectedTrack;

    // clip selection
    Val<processing::ClipKey> m_selectedClip;

    // data selection
    Val<std::vector<processing::TrackId>> m_selectedTrackIds;
    Val<processing::secs_t> m_selectedStartTime;
    Val<processing::secs_t> m_selectedEndTime;
};
}
