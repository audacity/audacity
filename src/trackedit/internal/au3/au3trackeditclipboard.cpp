/*
* Audacity: A Digital Audio Editor
*/

#include "au3trackeditclipboard.h"
#include "au3trackdata.h"

#include "containers.h"

#include "Track.h"
#include "WaveClip.h"
#include "WaveTrack.h"

using namespace au::trackedit;

std::vector<ITrackDataPtr> Au3TrackeditClipboard::trackDataCopy() const
{
    std::vector<Au3TrackDataPtr> deepCopiedTracksData;
    deepCopiedTracksData.reserve(m_tracksData.size());
    for (const auto& data : m_tracksData) {
        deepCopiedTracksData.push_back(std::make_shared<Au3TrackData>(data->track()->Duplicate()));
    }

    auto copiedGroupIds = getGroupIDs(m_tracksData);
    auto newGroupIds = createNewGroupIDs(copiedGroupIds);
    updateTracksDataWithIDs(deepCopiedTracksData, copiedGroupIds, newGroupIds);

    return { deepCopiedTracksData.begin(), deepCopiedTracksData.end() };
}

void Au3TrackeditClipboard::addTrackData(ITrackDataPtr trackData)
{
    const auto& au3TrackData = std::static_pointer_cast<Au3TrackData>(trackData);
    m_tracksData.push_back(std::move(au3TrackData));
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

std::set<int64_t> Au3TrackeditClipboard::getGroupIDs(const std::vector<Au3TrackDataPtr>& tracksData)
{
    std::set<int64_t> groupIds;

    for (const Au3TrackDataPtr& data : tracksData) {
        auto waveTrack = dynamic_cast<au3::Au3WaveTrack*>(data->track().get());
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

    int64_t startingID = 0;
    for (auto id : groupIDs) {
        newGroupIds.push_back(prj->createNewGroupID(startingID));
        startingID = newGroupIds.back() + 1; // + 1 or it would return the same repeatedly.
    }

    return newGroupIds;
}

void Au3TrackeditClipboard::updateTracksDataWithIDs(const std::vector<Au3TrackDataPtr>& tracksData,
                                                    const std::set<int64_t>& groupIDs,
                                                    const std::vector<int64_t>& newGroupIDs)
{
    IF_ASSERT_FAILED(groupIDs.size() == newGroupIDs.size());

    for (const Au3TrackDataPtr& data : tracksData) {
        auto waveTrack = dynamic_cast<au3::Au3WaveTrack*>(data->track().get());
        auto clips = waveTrack->Intervals();

        for (auto it = clips.begin(); it != clips.end(); ++it) {
            auto currentID = (*it).get()->GetGroupId();

            if (currentID != -1) {
                auto currentIDIterator = groupIDs.find(currentID);
                auto index = std::distance(groupIDs.begin(), currentIDIterator);

                // This private method should only be called from the same context as getGroupIDs and createGroupIDs
                // Or the data will not match.
                IF_ASSERT_FAILED(index >= 0);

                (*it).get()->SetGroupId(newGroupIDs[index]);
            }
        }
    }
}
