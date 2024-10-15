/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <vector>

#include "global/types/string.h"
#include "global/types/ratio.h"

class Effect;
class EffectInstanceEx;
namespace au::effects {
using ratio_t = muse::ratio_t;
using db_t = muse::db_t;
using rms_t = float;

using EffectId = muse::String;              // PluginID from AU3
using Effect = ::Effect;                    // Effect from AU3
using EffectInstanceId = uint64_t;
using EffectInstance = ::EffectInstanceEx;  // EffectInstanceEx from AU3

struct EffectMeta {
    EffectId id;
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

struct EffectTimeParams {
    double projectRate = 0.0;
    double t0 = 0.0;
    double t1 = 0.0;
    double f0 = 0.0;
    double f1 = 0.0;
};
}
