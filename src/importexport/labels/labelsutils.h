/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "io/path.h"

#include "au3-label-track/LabelTrack.h"

#include "labelstypes.h"

namespace au::importexport {
inline ::LabelFormat au3labelFormatFromSuffix(const muse::io::path_t& filePath)
{
    std::string suffix = muse::io::suffix(filePath);
    if (suffix == "srt") {
        return ::LabelFormat::SUBRIP;
    } else if (suffix == "vtt") {
        return ::LabelFormat::WEBVTT;
    }

    return ::LabelFormat::TEXT;
}

inline std::string fileSuffixFromType(FileType fileType)
{
    switch (fileType) {
        case FileType::TEXT:
            return "txt";
        case FileType::SUBRIP:
            return "srt";
        case FileType::WEBVTT:
            return "vtt";
        default:
            return "";
    }
    return "";
}
}
