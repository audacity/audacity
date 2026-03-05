/*
* Audacity: A Digital Audio Editor
*/
#include "clipboarddata.h"

using namespace au::trackedit;

std::vector<ITrackDataPtr> ClipboardData::trackData() const
{
    return m_tracksData;
}

void ClipboardData::addTrackData(ITrackDataPtr data)
{
    m_tracksData.push_back(std::move(data));
}

void ClipboardData::clearTrackData()
{
    m_tracksData.clear();
    m_isMultiSelectionCopy = false;
}

bool ClipboardData::trackDataEmpty() const
{
    return m_tracksData.empty();
}

size_t ClipboardData::trackDataSize() const
{
    return m_tracksData.size();
}

void ClipboardData::setMultiSelectionCopy(bool val)
{
    m_isMultiSelectionCopy = val;
}

bool ClipboardData::isMultiSelectionCopy() const
{
    return m_isMultiSelectionCopy;
}
