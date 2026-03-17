/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "trackedit/dom/clip.h"
#include "trackedit/dom/track.h"

namespace au::trackedit::utils {
bool hasStereoTrack(const TrackList& tracks);
std::vector<const Clip*> clipSetDifference(const Clips& set1, const Clips& set2);
}
