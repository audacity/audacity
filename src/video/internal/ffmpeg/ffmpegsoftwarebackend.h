/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_VIDEO_FFMPEGSOFTWAREBACKEND_H
#define AU_VIDEO_FFMPEGSOFTWAREBACKEND_H

#include <memory>
#include <string>

#include "../../ivideodecodebackend.h"

struct FFmpegFunctions;
class AVFormatContextWrapper;
class AVCodecContextWrapper;
class AVFrameWrapper;

namespace au::video {
//! Decodes video through the FFmpeg that Audacity loads at runtime.
//!
//! Synchronous and single-shot: every frameAt() seeks, flushes and decodes
//! forward. That is fast enough to follow a playhead being dragged while
//! stopped, which is all M1 needs, and it keeps the first version free of the
//! worker thread and frame cache.
//!
//! Must be constructed and used on one thread. FFmpegFunctions::Load() caches
//! into an unsynchronised static and mutates process environment on some
//! platforms, so it is called on the GUI thread only.
class FFmpegSoftwareBackend : public IVideoDecodeBackend
{
public:
    FFmpegSoftwareBackend();
    ~FFmpegSoftwareBackend() override;

    VideoError open(const std::string& path) override;
    void close() override;
    bool isOpen() const override;

    const VideoStreamInfo& streamInfo() const override;

    VideoFrame frameAt(muse::secs_t time, int targetWidth, int targetHeight) override;

    int64_t timeToPts(muse::secs_t time) const override;
    int64_t frameDurationPts() const override;

private:
    //! Content-relative seconds to a raw timestamp on the container timeline.
    int64_t toPts(muse::secs_t seconds) const;
    muse::secs_t toContentSeconds(int64_t pts) const;

    //! Keyframe seek backwards, flush, then decode forward to the frame whose
    //! presentation interval contains the target.
    bool seekAndDecode(int64_t targetPts);
    bool decodeUpTo(int64_t targetPts);
    void flushDecoder();

    std::shared_ptr<FFmpegFunctions> m_ffmpeg;
    std::unique_ptr<AVFormatContextWrapper> m_format;
    std::unique_ptr<AVCodecContextWrapper> m_codec;
    std::unique_ptr<AVFrameWrapper> m_frame;

    VideoStreamInfo m_info;
    int m_timeBaseNum = 0;
    int m_timeBaseDen = 1;
    int64_t m_defaultFrameDuration = 1;
    int m_sampleAspectNum = 0;
    int m_sampleAspectDen = 0;
    int64_t m_lastDecodedPts = 0;
    bool m_haveFrame = false;
    bool m_open = false;
};
}

#endif // AU_VIDEO_FFMPEGSOFTWAREBACKEND_H
