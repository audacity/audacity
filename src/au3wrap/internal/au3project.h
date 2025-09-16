/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>

#include "global/types/ret.h"
#include "global/async/notification.h"
#include "au3wrap/iau3project.h"
#include "libraries/lib-track/Track.h"
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
    muse::Ret load(const muse::io::path_t& filePath) override;
    bool save(const muse::io::path_t& fileName) override;
    void close() override;

    std::string title() const override;

    // Save status management
    [[nodiscard]] bool hasUnsavedChanges() const override;
    void markAsSaved() override;
    [[nodiscard]] bool isRecovered() const override;
    [[nodiscard]] bool isTemporary() const override;

    // Autosave management
    [[nodiscard]] bool hasAutosaveData() const override;
    bool removeAutosaveData() override;

    muse::async::Notification projectChanged() const override;

    // internal
    uintptr_t au3ProjectPtr() const override;

    void clearSavedState();
private:
    void updateSavedState();

    const std::shared_ptr<Au3ProjectData> m_data;
    Observer::Subscription mTrackListSubstription;
    Observer::Subscription mUndoSubscription;
    std::shared_ptr<TrackList> m_lastSavedTracks;
    muse::async::Notification m_projectChanged;
};

class Au3ProjectCreator : public IAu3ProjectCreator
{
public:

    std::shared_ptr<IAu3Project> create() const override;
    bool removeAutosaveDataFromFile(const muse::io::path_t& projectPath) const override;
};
}
