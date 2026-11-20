/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <string>

#include "framework/global/types/ret.h"

namespace au::projectrecovery {
//! Outcomes that the generic codes cannot tell apart; the text of the Ret holds the technical details
enum class Err {
    // The project module uses the start of the same range
    UnknownError = int(muse::Ret::Code::ProjectFirst) + 500,

    WriteFailed,
    NotAProject,
    NewerVersion,
    ResultFailedCheck,
};

inline muse::Ret make_ret(Err err, const std::string& text)
{
    return muse::Ret(static_cast<int>(err), text);
}
}
