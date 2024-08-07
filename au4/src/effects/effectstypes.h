/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <vector>

#include "global/types/string.h"
#include "global/io/path.h"
#include "global/types/val.h"

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

struct EffectParameter {
    muse::String id;
    muse::Val value;
    muse::Val defValue;
    muse::Val minValue;
    muse::Val maxValue;
    muse::Val scale;        //! todo: Scaling factor, for slider control

    EffectParameter() = default;
    EffectParameter(const muse::String& id, const muse::Val& value, const muse::Val& defValue,
                    const muse::Val& minValue, const muse::Val& maxValue, const muse::Val& scale)
        : id(id), value(value), defValue(defValue), minValue(minValue), maxValue(maxValue), scale(scale) {}
    EffectParameter(const muse::String& id, const muse::Val& value, const muse::Val& defValue)
        : id(id), value(value), defValue(defValue) {}
};
using EffectParameters = std::vector<EffectParameter>;
}
