/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "io/path.h"

#include "au3-label-track/LabelTrack.h"

namespace au::importexport {
inline ::LabelFormat labelFormatFromSuffix(const muse::io::path_t& filePath)
{
    std::string suffix = muse::io::suffix(filePath);
    if (suffix == "srt") {
        return ::LabelFormat::SUBRIP;
    } else if (suffix == "vtt") {
        return ::LabelFormat::WEBVTT;
    }

    return ::LabelFormat::TEXT;
}
}
