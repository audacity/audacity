/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "io/path.h"

namespace au::importexport {
struct FileInfo
{
    std::string filename;
    muse::io::path_t path;
    double duration;
};
} // namespace au::importexport
