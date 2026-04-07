/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <optional>
#include <string>

namespace au::importexport {
/// Extract BPM from a filename such as "120bpm_loop.wav" or "track-140 BPM.mp3".
std::optional<double> getBpmFromFilename(const std::string& filename);
}
