/*
* Audacity: A Digital Audio Editor
*/
#pragma once

namespace au::au4audio {
//! The chains and the device are stereo: a chain's buffers are always two, a mono track using the first only until it is
//! panned
constexpr int numChannels = 2;
}
