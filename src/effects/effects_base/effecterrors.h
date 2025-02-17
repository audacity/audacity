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
    InternalError = int(muse::Ret::Code::InternalError),
    UnknownError = EFFECTS_FIRST,

    EffectNotFound,
    EffectNoAudioSelected,
    EffectMultipleClipSelectionNotSupported,
    EffectProcessFailed,
    EffectProcessCancelled,

    // presets
    PresetNotValid,
    PresetMismatch
};

inline muse::Ret make_ret(Err e, std::string text = "")
{
    int retCode = static_cast<int>(e);

    switch (e) {
    case Err::Undefined: return muse::Ret(retCode);
    case Err::NoError: return muse::Ret(retCode);
    case Err::EffectProcessCancelled: return muse::Ret(muse::Ret::Code::Cancel);
    case Err::InternalError:
        return muse::Ret(retCode, muse::trc("effects", "Internal error"));
    case Err::UnknownError:
        return muse::Ret(retCode, text.empty() ? muse::trc("effects", "Unknown error") : text);
    case Err::EffectNotFound:
        return muse::Ret(retCode, text.empty() ? muse::trc("effects", "Effect not found") : text);
    case Err::EffectNoAudioSelected:
        return muse::Ret(retCode, text.empty() ? muse::trc("effects", "No audio selected") : text);
    case Err::EffectMultipleClipSelectionNotSupported:
        return muse::Ret(retCode, text.empty() ? muse::trc("effects", "Multiple-clip selection not supported") : text);
    case Err::EffectProcessFailed:
        return muse::Ret(retCode, text.empty() ? muse::trc("effects", "Applying effect failed") : text);
    case Err::PresetNotValid:
        return muse::Ret(retCode, text.empty() ? muse::trc("effects", "Not valid presets file") : text);
    case Err::PresetMismatch:
        return muse::Ret(retCode, text.empty() ? muse::trc("effects", "Preset mismatch") : text);
    }

    return muse::Ret(static_cast<int>(e));
}
} // namespace au::effects
