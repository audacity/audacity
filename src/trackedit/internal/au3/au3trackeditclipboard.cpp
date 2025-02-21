/*
* Audacity: A Digital Audio Editor
*/

#include "au3trackeditclipboard.h"

#include "containers.h"
#include "WaveClip.h"
#include "WaveTrack.h"

using namespace au::trackedit;

std::vector<TrackData> Au3TrackeditClipboard::trackDataSource() const
{
    return m_tracksData;
}

std::vector<TrackData> Au3TrackeditClipboard::trackDataCopy() const
{
    std::vector<TrackData> deepCopiedTracksData;
    deepCopiedTracksData.reserve(m_tracksData.size());

    for (const auto& i : m_tracksData) {
        deepCopiedTracksData.push_back(TrackData { i.track->Duplicate(), i.clipKey });
    }

    //! NOTE:: Checking if the copied data has group ID's,
    //         creating new ID's for the ones found,
    //         and updating the copied data with these.

    auto copiedGroupIds = getGroupIDs(deepCopiedTracksData);
    auto newGroupIds = createNewGroupIDs(copiedGroupIds);
    updateTracksDataWithIDs(deepCopiedTracksData, copiedGroupIds, newGroupIds);

    return deepCopiedTracksData;
}

TrackData Au3TrackeditClipboard::trackData(size_t i) const
{
    return m_tracksData.at(i);
}

void Au3TrackeditClipboard::clearTrackData()
{
    m_tracksData.clear();
    m_isMultiSelectionCopy = false;
}

bool Au3TrackeditClipboard::trackDataEmpty() const
{
    return m_tracksData.empty();
}

size_t Au3TrackeditClipboard::trackDataSize() const
{
    return m_tracksData.size();
}

void Au3TrackeditClipboard::addTrackData(const TrackData& trackData)
{
    m_tracksData.push_back(trackData);
}

void Au3TrackeditClipboard::setMultiSelectionCopy(bool newValue)
{
    if (m_isMultiSelectionCopy == newValue) {
        return;
    }

    m_isMultiSelectionCopy = newValue;
}

bool Au3TrackeditClipboard::isMultiSelectionCopy() const
{
    return m_isMultiSelectionCopy;
}

std::set<int64_t> Au3TrackeditClipboard::getGroupIDs(std::vector<TrackData>& tracksData)
{
    std::set<int64_t> groupIds;

    for (auto& data : tracksData) {
        auto waveTrack = dynamic_cast<au3::Au3WaveTrack*>(data.track.get());
        auto clips = waveTrack->Intervals();

        for (auto it = clips.begin(); it != clips.end(); ++it) {
            auto currentID = (*it).get()->GetGroupId();
            if (currentID != -1) {
                groupIds.emplace(currentID);
            }
        }
    }

    return groupIds;
}

std::vector<int64_t> Au3TrackeditClipboard::createNewGroupIDs(const std::set<int64_t>& groupIDs) const
{
    std::vector<int64_t> newGroupIds;

    auto prj = globalContext()->currentTrackeditProject();
    auto groupsList = prj->groupsIdsList();
    int64_t newGroupId = 0;

    for (auto id : groupIDs) {
        while (muse::contains(groupsList, newGroupId)) {
            newGroupId++;
        }
        newGroupIds.push_back(newGroupId);
        newGroupId++;
    }

    return newGroupIds;
}

void Au3TrackeditClipboard::updateTracksDataWithIDs(const std::vector<TrackData>& tracksData,
                                                    const std::set<int64_t>& groupIDs,
                                                    const std::vector<int64_t>& newGroupIDs)
{
    assert(groupIDs.size() == newGroupIDs.size());

    for (auto& data : tracksData) {
        auto waveTrack = dynamic_cast<au3::Au3WaveTrack*>(data.track.get());
        auto clips = waveTrack->Intervals();

        for (auto it = clips.begin(); it != clips.end(); ++it) {
            auto currentID = (*it).get()->GetGroupId();

            if (currentID != -1) {
                auto currentIDIterator = groupIDs.find(currentID);
                auto index = std::distance(groupIDs.begin(), currentIDIterator);

                // This private method should only be called from the same context as getGroupIDs and createGroupIDs
                // Or the data will not match.
                assert(index >= 0);

                (*it).get()->SetGroupId(newGroupIDs[index]);
            }
        }
    }
}
