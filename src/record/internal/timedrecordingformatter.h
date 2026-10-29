/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <chrono>

#include "framework/global/types/string.h"

namespace au::record {
class TimedRecordingFormatter
{
public:
    //! E.g. "1h 30m", "2d", "17h 36m 30s": zero units are left out, nothing is rounded
    static muse::String duration(std::chrono::seconds value);
};
}
