/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <vector>

#include "global/types/string.h"

namespace au::effects {
struct EffectMeta {
    muse::String id;
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

constexpr const char16_t* VST_CATEGORY_ID = u"vst";
constexpr const char16_t* BUILTIN_CATEGORY_ID = u"builtin";
}
