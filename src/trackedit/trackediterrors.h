/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "types/ret.h"
#include "translation.h"

namespace au::trackedit {
static constexpr int TRACKEDIT_FIRST = 6000; // TODO This has to go in framework's ret.h

enum class Err {
    Undefined       = int(muse::Ret::Code::Undefined),
    NoError         = int(muse::Ret::Code::Ok),
    UnknownError    = TRACKEDIT_FIRST,

    WaveTrackNotFound,
    ClipNotFound,
    TrackEmpty,
    NotEnoughSpaceForPaste,
    StereoClipIntoMonoTrack,
    FailedToMakeRoomForClip,
    NotEnoughDataInClipboard,
    DisallowedDuringRecording,
};

inline muse::Ret make_ret(Err e)
{
    int retCode = static_cast<int>(e);

    switch (e) {
    case Err::Undefined: return muse::Ret(retCode);
    case Err::NoError: return muse::Ret(retCode);
    case Err::UnknownError: return muse::Ret(retCode);
    case Err::WaveTrackNotFound: return muse::Ret(retCode);
    case Err::ClipNotFound: return muse::Ret(retCode);
    case Err::TrackEmpty: return muse::Ret(retCode);
    case Err::NotEnoughSpaceForPaste: return muse::Ret(retCode, muse::trc("trackedit", "Not enough space to paste clip into"));
    case Err::StereoClipIntoMonoTrack: return muse::Ret(retCode,
                                                        muse::trc("trackedit",
                                                                  "Stereo audio clips cannot be pasted onto mono tracks. "
                                                                  "Please convert the stereo clip to mono before pasting."));
    case Err::FailedToMakeRoomForClip: return muse::Ret(retCode);
    case Err::NotEnoughDataInClipboard: return muse::Ret(retCode);
    }

    return muse::Ret(static_cast<int>(e));
}
}
