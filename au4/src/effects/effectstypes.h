/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <vector>

#include "global/types/string.h"
#include "global/io/path.h"

namespace au::effects {
/*
manifest.json
{

"title": String,
"description": String,
"version": String,
"vendor": String,

"url": String                    // Name of qml file // todo
}*/

struct Manifest {
    muse::String id;
    muse::io::path_t url;
    muse::String title;
    muse::String description;
    muse::String version;
    muse::String vendor;

    bool isValid() const { return !id.empty() && !url.empty(); }
};

using ManifestList = std::vector<Manifest>;
}
