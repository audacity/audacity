/*
* Audacity: A Digital Audio Editor
*/

#include "au3trackeditclipboard.h"

using namespace au::trackedit;


std::vector<au::trackedit::TrackData> Au3TrackeditClipboard::trackData() const
{
    return m_tracksData;
}

void Au3TrackeditClipboard::clearTrackData()
{
    m_tracksData.clear();
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

void Au3TrackeditClipboard::eraseTrackData(std::vector<TrackData>::iterator begin, std::vector<TrackData>::iterator end)
{
    m_tracksData.erase(begin, end);
}
