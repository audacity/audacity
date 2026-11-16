/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>
#include <optional>
#include <string>

#include "framework/global/async/promise.h"
#include "framework/global/types/ret.h"
#include "framework/global/types/string.h"
#include "framework/global/types/translatablestring.h"
#include "framework/global/types/uri.h"
#include "framework/global/types/val.h"

namespace au::macromanager {
class IMacroStepType
{
public:
    virtual ~IMacroStepType() = default;

    virtual std::string id() const = 0;
    virtual muse::TranslatableString title() const = 0;
    virtual muse::String summary(const muse::Val& data) const = 0;
    virtual muse::Val defaultData() const = 0;
    virtual std::optional<muse::UriQuery> editUri() const = 0;
    virtual muse::async::Promise<muse::Ret> execute(const muse::Val& data) const = 0;
};

using IMacroStepTypePtr = std::shared_ptr<IMacroStepType>;
}
