/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QImage>

#include "global/io/path.h"

#include "../videopreviewtypes.h"

namespace au::videopreview {
struct VideoProbeResult
{
    VideoPreviewState state = VideoPreviewState::Empty;
    int streamIndex = -1;
    int streamId = -1;

    bool isUsable() const
    {
        return state == VideoPreviewState::Ready && streamIndex >= 0;
    }
};

struct VideoDecodeResult
{
    VideoPreviewState state = VideoPreviewState::Empty;
    QImage frame;
    double sourceTime = 0.0;

    bool isUsable() const
    {
        return state == VideoPreviewState::Ready && !frame.isNull();
    }
};

class FFmpegVideoDecoder
{
public:
    static VideoProbeResult probe(const muse::io::path_t& path, int preferredStreamIndex = -1, int preferredStreamId = -1);
    static VideoDecodeResult decodeFrame(const muse::io::path_t& path, int streamIndex, int streamId, double sourceSeconds);
};
}
