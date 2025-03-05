/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>

#include "../iau3project.h"
#include "Track.h"
#include "libraries/lib-utility/Observer.h"

class TrackList;
using TrackHolders = std::vector<std::shared_ptr<Track> >;

namespace au::au3 {
struct Au3ProjectData;
class Au3ProjectAccessor : public IAu3Project
{
public:

    Au3ProjectAccessor();

    void open() override;
    bool load(const muse::io::path_t& filePath) override;
    bool save(const muse::io::path_t& fileName) override;
    void close() override;

    bool import(const muse::io::path_t& filePath) override;

    std::string title() const override;

    // internal
    uintptr_t au3ProjectPtr() const override;

private:
    void updateSavedState();
    void addImportedTracks(const muse::io::path_t&, TrackHolders&& newTracks);

    const std::shared_ptr<Au3ProjectData> m_data;
    Observer::Subscription mTrackListSubstription;
    std::shared_ptr<TrackList> m_lastSavedTracks;
};

class Au3ProjectCreator : public IAu3ProjectCreator
{
public:

    std::shared_ptr<IAu3Project> create() const override;
};
}
