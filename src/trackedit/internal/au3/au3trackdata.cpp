/*
 * Audacity: A Digital Audio Editor
 */
#include "au3trackdata.h"

#include "libraries/lib-track/Track.h"

namespace au::trackedit {
Au3TrackData::Au3TrackData(std::shared_ptr<au3::Au3Track> track)
    : m_track{std::move(track)}
{
}
}
