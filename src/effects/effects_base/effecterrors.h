/*
 * Audacity: A Digital Audio Editor
 */

#pragma once

#include "translation.h"
#include "types/ret.h"

namespace au::effects {
static constexpr int EFFECTS_FIRST = 7000; // TODO This has to go in framework's ret.h
enum class Err {
    Undefined = int(muse::Ret::Code::Undefined),
    NoError = int(muse::Ret::Code::Ok),
    UnknownError = EFFECTS_FIRST,

    EffectProcessFailed,
};

inline muse::Ret make_ret(Err e, std::string text = "", std::string caption = "")
{
    int retCode = static_cast<int>(e);

    switch (e) {
    case Err::Undefined:
        return muse::Ret(retCode);
    case Err::NoError:
        return muse::Ret(retCode);
    case Err::UnknownError:
        return muse::Ret(retCode);
    case Err::EffectProcessFailed: {
        return muse::Ret(
            retCode,
            text.empty() ? muse::trc("effects", "Applying effect failed") : text);
    }
    }

    return muse::Ret(static_cast<int>(e));
}
} // namespace au::effects
