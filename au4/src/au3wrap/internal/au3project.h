/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>

#include "../iau3project.h"
#include "ProjectHistory.h"

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

    std::vector<trackedit::TrackId> trackIdList() const override;
    muse::async::NotifyList<trackedit::Track> trackList() const override;
    muse::async::NotifyList<trackedit::Clip> clipList(const trackedit::TrackId& trackId) const override;

    trackedit::TimeSignature timeSignature() const override;
    void setTimeSignature(const trackedit::TimeSignature& timeSignature) override;
    muse::async::Channel<au::trackedit::TimeSignature> timeSignatureChanged() const override;

    void pushHistoryState(const std::string& longDescription, const std::string& shortDescription) override;

    // internal
    uintptr_t au3ProjectPtr() const override;

private:

    std::shared_ptr<Au3ProjectData> m_data;
    mutable muse::async::ChangedNotifier<trackedit::Track> m_trackChangedNotifier;
    mutable muse::async::Channel<au::trackedit::TimeSignature> m_timeSignatureChanged;
};

class Au3ProjectCreator : public IAu3ProjectCreator
{
public:

    std::shared_ptr<IAu3Project> create() const override;
};
}
