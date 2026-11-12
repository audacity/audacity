/*
* Audacity: A Digital Audio Editor
*/
#pragma once

namespace au::au4audio {
//! What the device callback has played, for transportTime(): from `time`, `frames` contiguous frames
struct PlayedRange {
    double time = 0.0;
    unsigned long frames = 0;
};
}
