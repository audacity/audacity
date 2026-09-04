/*
* Audacity: A Digital Audio Editor
*/
#include "ffmpegsoftwarebackend.h"

#include <algorithm>
#include <cmath>
#include <filesystem>

#include <wx/string.h>

#include "mod-ffmpeg/lib-ffmpeg-support/FFmpegFunctions.h"
#include "mod-ffmpeg/lib-ffmpeg-support/wrappers/AVCodecContextWrapper.h"
#include "mod-ffmpeg/lib-ffmpeg-support/wrappers/AVFormatContextWrapper.h"
#include "mod-ffmpeg/lib-ffmpeg-support/wrappers/AVFrameWrapper.h"
#include "mod-ffmpeg/lib-ffmpeg-support/wrappers/AVPacketWrapper.h"
#include "mod-ffmpeg/lib-ffmpeg-support/wrappers/AVStreamWrapper.h"

#include "pixelconvert.h"

using namespace au::video;

namespace {
//! How far ahead it is worth decoding rather than seeking. Seeking costs a
//! flush and a re-decode from the previous keyframe, so for a short hop
//! forward it is usually cheaper to just keep going.
constexpr double FORWARD_WINDOW_SEC = 2.0;

//! Retries after an overshooting seek, backing off 1 s, 2 s then 4 s. Bounded
//! on purpose: this runs on the decode thread, which detach() joins.
constexpr int MAX_SEEK_ATTEMPTS = 3;
}

FFmpegSoftwareBackend::FFmpegSoftwareBackend() = default;

FFmpegSoftwareBackend::~FFmpegSoftwareBackend()
{
    close();
}

bool FFmpegSoftwareBackend::isOpen() const
{
    return m_open;
}

const VideoStreamInfo& FFmpegSoftwareBackend::streamInfo() const
{
    return m_info;
}

VideoError FFmpegSoftwareBackend::lastFrameError() const
{
    return m_lastFrameError;
}

int64_t FFmpegSoftwareBackend::timeToPts(muse::secs_t time) const
{
    return toPts(time);
}

int64_t FFmpegSoftwareBackend::frameDurationPts() const
{
    return m_currentFrameDuration;
}

void FFmpegSoftwareBackend::close()
{
    m_frame.reset();
    m_nextFrame.reset();
    m_codec.reset();
    m_format.reset();
    m_info = VideoStreamInfo();
    m_haveFrame = false;
    m_haveLookahead = false;
    m_lastFrameError = VideoError::None;
    m_open = false;
}

VideoError FFmpegSoftwareBackend::open(const std::string& path)
{
    close();

    m_ffmpeg = FFmpegFunctions::Load();
    if (!m_ffmpeg) {
        return VideoError::FFmpegNotFound;
    }

    m_format = m_ffmpeg->CreateAVFormatContext();
    if (!m_format) {
        return VideoError::FFmpegNotFound;
    }

    // Distinguish a file that is not there from one that cannot be decoded.
    // Without this both arrive as CannotOpen, and a project whose media has
    // moved is indistinguishable from a corrupt file.
    if (!std::filesystem::exists(std::filesystem::u8path(path))) {
        return VideoError::FileNotFound;
    }

    const wxString wxPath = wxString::FromUTF8(path.c_str());
    if (m_format->OpenInputContext(wxPath, nullptr, AVDictionaryWrapper(*m_ffmpeg))
        != AVIOContextWrapper::OpenResult::Success) {
        return VideoError::CannotOpen;
    }

    // First video stream that is not an attached cover image.
    const AVStreamWrapper* videoStream = nullptr;
    const AVStreamWrapper* audioStream = nullptr;
    const unsigned int count = m_format->GetStreamsCount();
    for (unsigned int i = 0; i < count; ++i) {
        const AVStreamWrapper* stream = m_format->GetStream(static_cast<int>(i));
        if (stream == nullptr) {
            continue;
        }

        if (stream->IsAudio()) {
            if (audioStream == nullptr) {
                audioStream = stream;
            }
            continue;
        }

        auto codecContext = stream->GetAVCodecContext();
        if (!codecContext
            || codecContext->GetCodecType() != AUDACITY_AVMEDIA_TYPE_VIDEO) {
            continue;
        }
        if (stream->GetDisposition() & AUDACITY_AV_DISPOSITION_ATTACHED_PIC) {
            continue;
        }
        if (videoStream == nullptr) {
            videoStream = stream;
        }
    }

    if (videoStream == nullptr) {
        return VideoError::NoVideoStream;
    }

    m_codec = videoStream->GetAVCodecContext();
    if (!m_codec) {
        return VideoError::NoDecoder;
    }

    // Video decoding needs the send/receive API. It is optional in the loader
    // and absent on avcodec 55, where audio import still works fine.
    if (!m_codec->CanDecodeVideo()) {
        return VideoError::FFmpegTooOld;
    }

    if (m_codec->GetCodec() == nullptr || m_codec->Open(m_codec->GetCodec()) < 0) {
        return VideoError::NoDecoder;
    }

    const AudacityAVRational timeBase = videoStream->GetTimeBase();
    m_timeBaseNum = timeBase.num;
    m_timeBaseDen = timeBase.den != 0 ? timeBase.den : 1;

    m_info.streamIndex = videoStream->GetIndex();

    // Recorded so the difference between the two streams is inspectable; the
    // anchor itself is the video start time, see toPts().
    if (audioStream != nullptr) {
        const AudacityAVRational audioBase = audioStream->GetTimeBase();
        const int64_t audioStart = audioStream->GetStartTime();
        if (audioStart != AUDACITY_AV_NOPTS_VALUE && audioBase.den != 0) {
            m_info.audioStartTime = static_cast<double>(audioStart)
                                    * audioBase.num / audioBase.den;
        }
    }

    // The anchor. Set before anything calls toPts() or toContentSeconds().
    const int64_t videoStart = videoStream->GetStartTime();
    m_info.videoStartTime = videoStart == AUDACITY_AV_NOPTS_VALUE
                            ? muse::secs_t(0.0)
                            : muse::secs_t(static_cast<double>(videoStart)
                                           * m_timeBaseNum / m_timeBaseDen);

    // Known as soon as the codec is open, so the panel can report the size
    // without waiting for a frame; the decode now happens on another thread.
    m_info.width = m_codec->GetVideoWidth();
    m_info.height = m_codec->GetVideoHeight();

    const AudacityAVRational sar = videoStream->GetSampleAspectRatio();
    m_sampleAspectNum = sar.num;
    m_sampleAspectDen = sar.den;

    const AudacityAVRational rate = videoStream->GetAvgFrameRate();
    m_info.frameRate = rate.den != 0 ? static_cast<double>(rate.num) / rate.den : 0.0;

    // A fallback only, for the final frame before end of stream and for files
    // that report no frame rate. Frame selection measures each frame's real
    // end from the next frame's timestamp instead: a computed 1/fps is wrong
    // on variable frame rate material and slightly wrong even on constant rate
    // material whose real deltas alternate, as 30000/1001 does.
    m_defaultFrameDuration = 1;
    if (m_info.frameRate > 0.0) {
        m_defaultFrameDuration = std::max<int64_t>(
            1, llround(1.0 / m_info.frameRate * m_timeBaseDen / m_timeBaseNum));
    }

    const int64_t duration = videoStream->GetDuration();
    if (duration != AUDACITY_AV_NOPTS_VALUE && duration > 0) {
        m_info.duration = static_cast<double>(duration) * m_timeBaseNum / m_timeBaseDen;
    } else {
        // Matroska, among others, stores no per-stream duration. The container
        // level one is in AV_TIME_BASE units regardless of any stream's time
        // base. Without this the range check is inert and seeking past the end
        // silently leaves the last decoded frame on screen.
        const int64_t containerDuration = m_format->GetDuration();
        if (containerDuration > 0) {
            m_info.duration = static_cast<double>(containerDuration) / AUDACITY_AV_TIME_BASE;
        }
    }

    m_frame = m_ffmpeg->CreateAVFrameWrapper();
    m_nextFrame = m_ffmpeg->CreateAVFrameWrapper();
    if (!m_frame || !m_nextFrame) {
        return VideoError::DecodeFailed;
    }

    m_open = true;
    m_haveFrame = false;
    return VideoError::None;
}

//! Content-relative seconds to a timestamp on the container timeline.
//!
//! The anchor is the video stream's start time. Frame timestamps live on the
//! container timeline, and content frame zero sits at that start time, so
//! subtracting it maps project time onto the picture.
//!
//! The audio stream's start time is deliberately not used, even though it is
//! the audio that Audacity imported. Within one container the two differ by
//! the encoder priming - 21.33 ms for AAC at 48 kHz - because the audio stream
//! begins with priming samples that libavformat strips while demuxing, via the
//! edit list in MP4. Once those are stripped the first imported sample lines up
//! with the video start, not with the raw audio start, and anchoring on the
//! latter shifts the picture by half a frame or more.
//!
//! The residual is the importer's, not this code's. On MPEG-TS the two streams
//! start 21 ms apart AND the importer prepends silence it should not, so the
//! observable gap is about 153 ms rather than 21 ms. Correcting the importer
//! is a change to where every AAC and MPEG-TS import lands on the timeline and
//! belongs to whoever owns the importer; see tools/videosync/README.md.
int64_t FFmpegSoftwareBackend::toPts(muse::secs_t seconds) const
{
    const double base = static_cast<double>(m_timeBaseNum) / m_timeBaseDen;
    const double absolute = seconds.to_double() + m_info.videoStartTime.to_double();
    return static_cast<int64_t>(llround(absolute / base));
}

muse::secs_t FFmpegSoftwareBackend::toContentSeconds(int64_t pts) const
{
    const double absolute = static_cast<double>(pts) * m_timeBaseNum / m_timeBaseDen;
    return absolute - m_info.videoStartTime.to_double();
}

void FFmpegSoftwareBackend::flushDecoder()
{
    if (m_codec) {
        m_codec->FlushBuffers();
    }
    m_haveFrame = false;

    // Anything carried over belongs to the position we just left.
    m_haveLookahead = false;
}

//! Where to retry a seek from, having overshot. Doubling, clamped to the
//! start of the stream. Free-standing and arithmetic-only so the two
//! non-termination modes can be tested without a file.
int64_t FFmpegSoftwareBackend::nextProbePts(int64_t targetPts, int attempt,
                                            int64_t floorPts, int64_t ticksPerSecond,
                                            bool* atFloor)
{
    const int64_t backoffSeconds = static_cast<int64_t>(1) << attempt;   // 1, 2, 4
    int64_t probe = targetPts - backoffSeconds * ticksPerSecond;

    const bool clamped = probe <= floorPts;
    if (clamped) {
        probe = floorPts;
    }
    if (atFloor != nullptr) {
        *atFloor = clamped;
    }
    return probe;
}

bool FFmpegSoftwareBackend::decodeUpTo(int64_t targetPts, int64_t* firstDecodedPts)
{
    const auto timestampOf = [](AVFrameWrapper& frame) {
        int64_t pts = frame.GetBestEffortTimestamp();
        if (pts == AUDACITY_AV_NOPTS_VALUE) {
            pts = frame.GetPresentationTimestamp();
        }
        return pts;
    };

    bool haveHeld = false;
    int64_t heldPts = 0;
    bool draining = false;

    const auto takeHeld = [&](int64_t pts) {
        m_frame.swap(m_nextFrame);
        heldPts = pts;
        haveHeld = true;
        m_lastDecodedPts = pts;
        m_haveFrame = true;
    };

    const auto answer = [&](int64_t nextPts) {
        // The held frame starts at or before the target and the next one
        // starts after it, so the held frame is the answer and the gap
        // between them is its true duration.
        m_currentFrameDuration = std::max<int64_t>(1, nextPts - heldPts);
        m_lastDecodedPts = heldPts;
        m_haveFrame = true;
    };

    while (true) {
        int64_t pts = 0;

        if (m_haveLookahead) {
            // Left over from the previous search. Dropping it would cost a
            // frame on every forward step.
            pts = m_lookaheadPts;
            m_haveLookahead = false;
        } else {
            if (m_codec->ReceiveFrame(*m_nextFrame) != 0) {
                if (draining) {
                    break;   // nothing left anywhere
                }

                auto packet = m_format->ReadNextPacket();
                if (!packet) {
                    m_codec->SendPacket(nullptr);
                    draining = true;
                    continue;
                }

                if (packet->GetStreamIndex() == m_info.streamIndex) {
                    m_codec->SendPacket(packet.get());
                }
                continue;
            }
            pts = timestampOf(*m_nextFrame);
        }

        if (firstDecodedPts != nullptr) {
            *firstDecodedPts = pts;
            firstDecodedPts = nullptr;   // only the first one after a flush
        }

        if (haveHeld && pts > targetPts) {
            // Keep this one: it is the first frame of the next search.
            m_haveLookahead = true;
            m_lookaheadPts = pts;
            answer(pts);
            return true;
        }

        takeHeld(pts);
    }

    if (haveHeld) {
        // End of stream, so there is no next frame to measure against and the
        // last one falls back to the nominal duration. Its timestamp is
        // recorded too; the previous version left it unset, so the drained
        // image was paired with the preceding frame's time.
        m_currentFrameDuration = m_defaultFrameDuration;
        m_lastDecodedPts = heldPts;
        m_haveFrame = true;
        return true;
    }

    return false;
}

bool FFmpegSoftwareBackend::seekAndDecode(int64_t targetPts)
{
    m_seekAttempts = 0;

    const double base = static_cast<double>(m_timeBaseNum) / m_timeBaseDen;
    const int64_t forwardWindow = static_cast<int64_t>(FORWARD_WINDOW_SEC / base);
    const int64_t ticksPerSecond =
        std::max<int64_t>(1, static_cast<int64_t>(llround(1.0 / base)));

    const bool canDecodeForward = m_haveFrame
                                  && targetPts >= m_lastDecodedPts
                                  && targetPts - m_lastDecodedPts <= forwardWindow;

    if (canDecodeForward) {
        // Already at or before the target, so decoding forward cannot overshoot
        // and there is nothing to verify.
        return decodeUpTo(targetPts, nullptr);
    }

    if (m_ffmpeg->av_seek_frame == nullptr) {
        return false;
    }

    AVFormatContext* format = m_format->GetWrappedValue();
    const int64_t floorPts = toPts(muse::secs_t(0.0));

    int64_t seekTo = targetPts;

    for (int attempt = 0; attempt <= MAX_SEEK_ATTEMPTS; ++attempt) {
        m_ffmpeg->av_seek_frame(format, m_info.streamIndex, seekTo,
                                AUDACITY_AVSEEK_FLAG_BACKWARD);
        flushDecoder();
        ++m_seekAttempts;

        int64_t firstPts = AUDACITY_AV_NOPTS_VALUE;
        const bool decoded = decodeUpTo(targetPts, &firstPts);

        // Decoding nothing at all is the same problem as overshooting, and it
        // is what happens near the end of a file when the seek lands past the
        // last frame. Back off rather than giving up; the attempt counter
        // bounds this either way.
        if (decoded && (firstPts == AUDACITY_AV_NOPTS_VALUE || firstPts <= targetPts)) {
            // Landed at or before the target, so decoding forward reached the
            // right frame. Landing early is only slower, never wrong; only
            // landing late is wrong, and that is what this detects.
            return true;
        }

        // Overshot. MPEG-TS is the case that needs this: av_seek_frame is
        // exact on the decode timestamp but blind to keyframes, so the
        // decoder discards forward to the next one and the first frame it
        // emits can be a whole group of pictures past the target. Retry from
        // further back; the forward walk then reaches the right frame.
        if (attempt == MAX_SEEK_ATTEMPTS) {
            break;
        }

        bool atFloor = false;
        seekTo = nextProbePts(targetPts, attempt, floorPts, ticksPerSecond, &atFloor);

        if (atFloor) {
            // The start of the stream still overshoots, which means timestamp
            // seeking cannot reach the first keyframe. Rewinding by byte is
            // the last resort - and only here, because using it generally
            // regresses containers that seek correctly by timestamp.
            m_ffmpeg->av_seek_frame(format, -1, 0, AUDACITY_AVSEEK_FLAG_BYTE);
            flushDecoder();
            ++m_seekAttempts;

            int64_t fromStart = AUDACITY_AV_NOPTS_VALUE;
            return decodeUpTo(targetPts, &fromStart);
        }
    }

    // Out of attempts. The held frame is the closest reachable one, which is
    // better than showing nothing.
    return m_haveFrame;
}

VideoFrame FFmpegSoftwareBackend::frameAt(muse::secs_t time,
                                          int targetWidth, int targetHeight)
{
    VideoFrame result;
    m_lastFrameError = VideoError::None;

    if (!m_open || targetWidth <= 0 || targetHeight <= 0) {
        return result;
    }

    if (!seekAndDecode(toPts(time))) {
        m_lastFrameError = VideoError::DecodeFailed;
        return result;
    }

    const uint8_t* data[3] = {
        m_frame->GetData(0), m_frame->GetData(1), m_frame->GetData(2)
    };
    const int lineSize[3] = {
        m_frame->GetLineSize(0), m_frame->GetLineSize(1), m_frame->GetLineSize(2)
    };

    const int srcWidth = m_frame->GetWidth();
    const int srcHeight = m_frame->GetHeight();
    if (srcWidth <= 0 || srcHeight <= 0) {
        m_lastFrameError = VideoError::DecodeFailed;
        return result;
    }

    // A high dynamic range frame decoded as though it were ordinary gamma
    // renders at roughly half brightness - reference white lands on middle
    // grey - and looks merely dark rather than obviously broken. Refuse it
    // and say so instead of showing something quietly wrong.
    const AudacityAVColorTransfer transfer = m_frame->GetColorTransfer();
    if (transfer == AUDACITY_AVCOL_TRC_SMPTE2084
        || transfer == AUDACITY_AVCOL_TRC_ARIB_STD_B67) {
        m_lastFrameError = VideoError::UnsupportedHdr;
        return result;
    }

    if (!isSupportedPixelFormat(m_frame->GetPixelFormat())) {
        m_lastFrameError = VideoError::UnsupportedFormat;
        return result;
    }

    // Fit inside the requested box rather than filling it. Anamorphic material
    // - DV and HDV in particular - stores non-square pixels, so the shape on
    // screen comes from the sample aspect ratio and not from the stored
    // dimensions; ignoring it squashes exactly the formats this feature is
    // most likely to be pointed at.
    double displayWidth = srcWidth;
    if (m_sampleAspectNum > 0 && m_sampleAspectDen > 0) {
        displayWidth = srcWidth * static_cast<double>(m_sampleAspectNum) / m_sampleAspectDen;
    }

    const double scale = std::min(targetWidth / displayWidth,
                                  static_cast<double>(targetHeight) / srcHeight);
    const int outWidth = std::max(1, static_cast<int>(llround(displayWidth * scale)));
    const int outHeight = std::max(1, static_cast<int>(llround(srcHeight * scale)));

    result.image = yuv420ToImage(data, lineSize,
                                 srcWidth, srcHeight,
                                 outWidth, outHeight,
                                 m_frame->GetPixelFormat(),
                                 m_frame->GetColorSpace(),
                                 m_frame->GetColorRange());
    if (result.image.isNull()) {
        m_lastFrameError = VideoError::UnsupportedFormat;
        return result;
    }

    result.pts = m_lastDecodedPts;
    result.time = toContentSeconds(m_lastDecodedPts);

    return result;
}
