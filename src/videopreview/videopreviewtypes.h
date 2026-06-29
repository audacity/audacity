/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <vector>

#include "global/io/path.h"
#include "global/types/string.h"
#include "trackedit/dom/clip.h"
#include "trackedit/dom/track.h"

namespace au::videopreview {
enum class VideoPreviewState
{
    Empty,
    Ready,
    MissingFile,
    NoFfmpeg,
    UnsupportedCodec,
    DecodingError
};

struct VideoSegment
{
    trackedit::ClipKey clipKey;
    muse::String title;
    int groupId = -1;
    trackedit::ClipColorIndex colorIndex = 4;
    double projectStart = 0.0;
    double projectEnd = 0.0;
    double sourceStart = 0.0;
    double sourceEnd = 0.0;

    bool isValid() const
    {
        return clipKey.isValid() && projectEnd > projectStart && sourceEnd > sourceStart;
    }
};

struct VideoLink
{
    trackedit::TrackId trackId = trackedit::INVALID_TRACK;
    muse::io::path_t sourcePath;
    muse::String trackTitle;
    int streamIndex = -1;
    int streamId = -1;
    std::vector<VideoSegment> segments;

    bool isValid() const
    {
        return trackId != trackedit::INVALID_TRACK && !sourcePath.empty() && streamIndex >= 0 && !segments.empty();
    }
};

using VideoLinks = std::vector<VideoLink>;
}
