/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <vector>

#include "global/types/string.h"
#include "global/io/path.h"

namespace au::effects {
struct EffectMeta {
    muse::String id;
    muse::io::path_t url;
    muse::String title;
    muse::String description;
    muse::String version;
    muse::String vendor;

    muse::String categoryId;

    bool isValid() const { return !id.empty(); }
};

using EffectMetaList = std::vector<EffectMeta>;

struct EffectCategory {
    muse::String id;
    muse::String title;

    bool isValid() const { return !id.empty(); }
};

using EffectCategoryList = std::vector<EffectCategory>;
}
