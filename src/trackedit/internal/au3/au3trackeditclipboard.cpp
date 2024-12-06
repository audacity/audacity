/*
* Audacity: A Digital Audio Editor
*/

#include "au3trackeditclipboard.h"

using namespace au::trackedit;


std::vector<au::trackedit::TrackData> Au3TrackeditClipboard::trackDataSource() const
{
    return m_tracksData;
}

std::vector<TrackData> Au3TrackeditClipboard::trackDataCopy() const
{
    std::vector<TrackData> deepCopiedTracksData;

    for (int i = 0; i < m_tracksData.size(); ++i) {
        deepCopiedTracksData.push_back(TrackData { m_tracksData.at(i).track->Duplicate(), m_tracksData.at(i).clipKey});
    }

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
