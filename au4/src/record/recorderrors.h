/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "types/ret.h"
#include "translation.h"

namespace au::record {
static constexpr int RECORD_FIRST = 5000;

enum class Err {
    Undefined       = int(muse::Ret::Code::Undefined),
    NoError         = int(muse::Ret::Code::Ok),
    UnknownError    = RECORD_FIRST,

    RecordingError,
    RecordingStopError,
    MismatchedSamplingRatesError,
    TooFewCompatibleTracksSelected
};

inline muse::Ret make_ret(Err e)
{
    int retCode = static_cast<int>(e);

    switch (e) {
    case Err::Undefined: return muse::Ret(retCode);
    case Err::NoError: return muse::Ret(retCode);
    case Err::UnknownError: return muse::Ret(retCode);
    case Err::RecordingError: return muse::Ret(retCode, muse::trc("record", "Error opening recording device.\nError code: %1"));
    case Err::RecordingStopError: return muse::Ret(retCode, muse::trc("record", "Cannot stop recording"));
    case Err::MismatchedSamplingRatesError: return muse::Ret(retCode,
                                                             muse::trc("record",
                                                                       "The tracks selected for recording must all have the same sampling rate"));
    case Err::TooFewCompatibleTracksSelected: return muse::Ret(retCode,
                                                               muse::trc("record",
                                                                         "Too few tracks are selected for recording at this sample rate.\n"
                                                                         "(Audacity requires two channels at the same sample rate foreach stereo track)"));
    }

    return muse::Ret(static_cast<int>(e));
}
}
