/*
* Audacity: A Digital Audio Editor
*/
#include "ffmpegsoftwarebackend.h"

#include <algorithm>
#include <cmath>

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

void FFmpegSoftwareBackend::close()
{
    m_frame.reset();
    m_codec.reset();
    m_format.reset();
    m_info = VideoStreamInfo();
    m_haveFrame = false;
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

    const AudacityAVRational sar = videoStream->GetSampleAspectRatio();
    m_sampleAspectNum = sar.num;
    m_sampleAspectDen = sar.den;

    const AudacityAVRational rate = videoStream->GetAvgFrameRate();
    m_info.frameRate = rate.den != 0 ? static_cast<double>(rate.num) / rate.den : 0.0;

    // Frame duration is only a fallback for containers that do not carry a
    // packet duration. Variable frame rate content legitimately holds a single
    // frame for seconds, so a computed 1/fps is never used to decide staleness.
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
    if (!m_frame) {
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
//! The residual case is a container that carries priming with no edit list to
//! strip it, MPEG-TS being the common one. There the sound is late by the
//! priming duration relative to the picture; that is the importer's error to
//! fix rather than one to compensate for here, and it is bounded at ~21 ms.
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
}

bool FFmpegSoftwareBackend::decodeUpTo(int64_t targetPts)
{
    while (true) {
        const int received = m_codec->ReceiveFrame(*m_frame);
        if (received == 0) {
            int64_t pts = m_frame->GetBestEffortTimestamp();
            if (pts == AUDACITY_AV_NOPTS_VALUE) {
                pts = m_frame->GetPresentationTimestamp();
            }
            m_lastDecodedPts = pts;
            m_haveFrame = true;

            if (pts + m_defaultFrameDuration > targetPts) {
                return true;
            }
            continue;
        }

        // Needs more input. Feed it the next packet belonging to our stream.
        auto packet = m_format->ReadNextPacket();
        if (!packet) {
            m_codec->SendPacket(nullptr);   // drain
            const int drained = m_codec->ReceiveFrame(*m_frame);
            if (drained == 0) {
                m_haveFrame = true;
                return true;
            }
            return m_haveFrame;
        }

        if (packet->GetStreamIndex() == m_info.streamIndex) {
            m_codec->SendPacket(packet.get());
        }
    }
}

bool FFmpegSoftwareBackend::seekAndDecode(int64_t targetPts)
{
    const double base = static_cast<double>(m_timeBaseNum) / m_timeBaseDen;
    const int64_t forwardWindow =
        static_cast<int64_t>(FORWARD_WINDOW_SEC / base);

    const bool canDecodeForward = m_haveFrame
                                  && targetPts >= m_lastDecodedPts
                                  && targetPts - m_lastDecodedPts <= forwardWindow;

    if (!canDecodeForward) {
        if (m_ffmpeg->av_seek_frame == nullptr) {
            return false;
        }
        m_ffmpeg->av_seek_frame(m_format->GetWrappedValue(), m_info.streamIndex,
                                targetPts, AUDACITY_AVSEEK_FLAG_BACKWARD);
        flushDecoder();
    }

    return decodeUpTo(targetPts);
}

VideoFrame FFmpegSoftwareBackend::frameAt(muse::secs_t time,
                                          int targetWidth, int targetHeight)
{
    VideoFrame result;
    if (!m_open || targetWidth <= 0 || targetHeight <= 0) {
        return result;
    }

    if (!seekAndDecode(toPts(time))) {
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
    result.pts = m_lastDecodedPts;
    result.time = toContentSeconds(m_lastDecodedPts);

    if (m_info.width == 0) {
        m_info.width = m_frame->GetWidth();
        m_info.height = m_frame->GetHeight();
    }

    return result;
}
