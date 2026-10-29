/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <string>
#include <vector>

#include "framework/global/io/path.h"

namespace au::macromanager {
std::vector<std::string> uniqueOutputNames(const muse::io::paths_t& files);
}
