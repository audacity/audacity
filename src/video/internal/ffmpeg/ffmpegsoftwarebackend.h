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

    VideoError lastFrameError() const override;
    int64_t timeToPts(muse::secs_t time) const override;
    int64_t frameDurationPts() const override;

private:
    //! Content-relative seconds to a raw timestamp on the container timeline.
    int64_t toPts(muse::secs_t seconds) const;
    muse::secs_t toContentSeconds(int64_t pts) const;

    //! Seek backwards, flush, decode forward - then check the result and
    //! retry from further back if the seek overshot the target.
    bool seekAndDecode(int64_t targetPts);

    //! Decodes forward until the frame covering targetPts. Holds one frame of
    //! lookahead, so a frame's end is the next frame's timestamp rather than a
    //! guess from the average frame rate.
    //!
    //! firstDecodedPts reports the timestamp of the first frame produced after
    //! the last flush. It is how the caller learns that the seek overshot; by
    //! the time this returns, the loop has already walked past it.
    bool decodeUpTo(int64_t targetPts, int64_t* firstDecodedPts);

    void flushDecoder();

    //! How far before the target to retry from, per attempt. Public for the
    //! unit tests, which drive the ladder without touching a file.
public:
    static int64_t nextProbePts(int64_t targetPts, int attempt, int64_t floorPts, int64_t ticksPerSecond, bool* atFloor);

    //! Seeks issued for the most recent request. One means the first seek
    //! landed correctly; more means the retry ladder ran.
    int seekAttempts() const { return m_seekAttempts; }

private:

    std::shared_ptr<FFmpegFunctions> m_ffmpeg;
    std::unique_ptr<AVFormatContextWrapper> m_format;
    std::unique_ptr<AVCodecContextWrapper> m_codec;
    std::unique_ptr<AVFrameWrapper> m_frame;

    //! One frame of lookahead. A frame's true end is the next frame's
    //! timestamp; anything derived from the average frame rate is wrong on
    //! variable frame rate material and slightly wrong even on constant rate
    //! material whose real deltas alternate.
    //!
    //! It has to survive between requests. The frame that ends one search is
    //! the first frame of the next one, and dropping it costs exactly one
    //! frame on every forward step.
    std::unique_ptr<AVFrameWrapper> m_nextFrame;
    bool m_haveLookahead = false;
    int64_t m_lookaheadPts = 0;

    VideoStreamInfo m_info;
    int m_timeBaseNum = 0;
    int m_timeBaseDen = 1;
    //! Only a fallback now: the final frame before end of stream, and files
    //! that report no frame rate at all.
    int64_t m_defaultFrameDuration = 1;

    //! True duration of the frame currently held, measured from the next
    //! frame's timestamp.
    int64_t m_currentFrameDuration = 1;
    int m_seekAttempts = 0;
    VideoError m_lastFrameError = VideoError::None;
    int m_sampleAspectNum = 0;
    int m_sampleAspectDen = 0;
    int64_t m_lastDecodedPts = 0;
    bool m_haveFrame = false;
    bool m_open = false;
};
}

#endif // AU_VIDEO_FFMPEGSOFTWAREBACKEND_H
