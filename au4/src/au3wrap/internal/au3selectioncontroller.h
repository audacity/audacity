/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "processing/iselectioncontroller.h"

namespace au::au3 {
class Au3SelectionController : public processing::ISelectionController
{
public:
    Au3SelectionController() = default;

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

    // clip selection
    Val<processing::ClipKey> m_selectedClip;

    // data selection
    Val<std::vector<processing::TrackId>> m_selectedTrackIds;
    Val<processing::secs_t> m_selectedStartTime;
    Val<processing::secs_t> m_selectedEndTime;
};
}
