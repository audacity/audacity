/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "types/ret.h"
#include "translation.h"

namespace au::trackedit {
enum class Err {
    Undefined       = int(muse::Ret::Code::Undefined),
    NoError         = int(muse::Ret::Code::Ok),
    UnknownError    = int(muse::Ret::Code::UnknownError),

    WaveTrackNotFound,
    TrackEmpty,
    NotEnoughSpaceForPaste,
    StereoClipIntoMonoTrack
};

inline muse::Ret make_ret(Err e)
{
    int retCode = static_cast<int>(e);

    switch (e) {
    case Err::Undefined: return muse::Ret(retCode);
    case Err::NoError: return muse::Ret(retCode);
    case Err::UnknownError: return muse::Ret(retCode);
    case Err::WaveTrackNotFound: return muse::Ret(retCode);
    case Err::TrackEmpty: return muse::Ret(retCode);
    case Err::NotEnoughSpaceForPaste: return muse::Ret(retCode, muse::trc("trackedit", "Not enough space to paste clip into"));
    case Err::StereoClipIntoMonoTrack: return muse::Ret(retCode,
                         muse::trc("trackedit",
                                   "The content you are trying to paste will span across more tracks than you"
                                   " currently have available. Add more tracks and try again."));
    }

    return muse::Ret(static_cast<int>(e));
}
}
