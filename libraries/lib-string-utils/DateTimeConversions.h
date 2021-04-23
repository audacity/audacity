#pragma once

#include <chrono>
#include <string>

#include "StringUtilsApi.h"

namespace audacity
{
using SystemTime = std::chrono::system_clock::time_point;

STRING_UTILS_API bool ParseRFC822Date (const std::string& dateString, SystemTime* time);
STRING_UTILS_API std::string SerializeRFC822Date (SystemTime timePoint);
}