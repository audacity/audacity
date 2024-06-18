/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>

#include "../iau3project.h"

namespace au::au3 {
struct Au3ProjectData;
class Au3Project : public IAu3Project
{
public:

    Au3Project();

    static std::shared_ptr<Au3Project> create();

    bool load(const muse::io::path_t& filePath) override;
    bool save(const muse::io::path_t& fileName) override;
    void close() override;

    std::string title() const override;

    std::vector<processing::TrackId> trackIdList() const override;
    muse::async::NotifyList<processing::Track> trackList() const override;
    muse::async::NotifyList<processing::Clip> clipList(const processing::TrackId& trackId) const override;

    processing::TimeSignature timeSignature() const override;
    muse::async::Channel<au::processing::TimeSignature> timeSignatureChanged() const override;

    // internal
    uintptr_t au3ProjectPtr() const override;

private:

    std::shared_ptr<Au3ProjectData> m_data;
    mutable muse::async::ChangedNotifier<processing::Track> m_trackChangedNotifier;
    mutable muse::async::Channel<au::processing::TimeSignature> m_timeSignatureChanged;
};

class Au3ProjectCreator : public IAu3ProjectCreator
{
public:

    std::shared_ptr<IAu3Project> create() const override;
};
}
