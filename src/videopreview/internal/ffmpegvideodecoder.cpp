/*
* Audacity: A Digital Audio Editor
*/
#include "ffmpegvideodecoder.h"

#include <algorithm>
#include <array>
#include <cmath>

#include <wx/string.h>

#include "mod-ffmpeg/lib-ffmpeg-support/FFmpegFunctions.h"

using namespace au::videopreview;

namespace {
constexpr int MAX_PACKETS_TO_DECODE = 800;
constexpr double FRAME_TIME_TOLERANCE = 1.0 / 120.0;

double secondsFromTimestamp(int64_t timestamp, AudacityAVRational timeBase)
{
    if (timestamp == AUDACITY_AV_NOPTS_VALUE || timeBase.den == 0) {
        return 0.0;
    }

    return static_cast<double>(timestamp) * static_cast<double>(timeBase.num) / static_cast<double>(timeBase.den);
}

int64_t timestampFromSeconds(double seconds, AudacityAVRational timeBase)
{
    if (timeBase.num == 0) {
        return 0;
    }

    return static_cast<int64_t>(std::floor(seconds * static_cast<double>(timeBase.den) / static_cast<double>(timeBase.num)));
}

int64_t streamStartTimestamp(const AVStreamWrapper& stream)
{
    const int64_t startTime = stream.GetStartTime();
    return startTime == AUDACITY_AV_NOPTS_VALUE ? 0 : startTime;
}

double sourceSecondsFromTimestamp(int64_t timestamp, int64_t streamStart, AudacityAVRational timeBase)
{
    if (timestamp == AUDACITY_AV_NOPTS_VALUE) {
        return 0.0;
    }

    return secondsFromTimestamp(timestamp - streamStart, timeBase);
}

struct OpenVideoContext
{
    std::shared_ptr<FFmpegFunctions> ffmpeg;
    std::unique_ptr<AVFormatContextWrapper> formatContext;
    const AVStreamWrapper* stream = nullptr;
    int streamIndex = -1;
};

OpenVideoContext openVideoContext(const muse::io::path_t& path, int preferredStreamIndex, int preferredStreamId)
{
    OpenVideoContext result;
    result.ffmpeg = FFmpegFunctions::Load();

    if (!result.ffmpeg || !result.ffmpeg->SupportsVideoDecode()) {
        return result;
    }

    result.formatContext = result.ffmpeg->CreateAVFormatContext();
    if (!result.formatContext) {
        return {};
    }

    const auto openResult = result.formatContext->OpenInputContext(
        wxString::FromUTF8(path.toStdString().c_str()), nullptr, AVDictionaryWrapper(*result.ffmpeg));
    if (openResult != AVIOContextWrapper::OpenResult::Success) {
        return {};
    }

    const AVStreamWrapper* firstVideoStream = nullptr;

    for (unsigned int i = 0; i < result.formatContext->GetStreamsCount(); ++i) {
        const AVStreamWrapper* stream = result.formatContext->GetStream(static_cast<int>(i));
        if (!stream || !stream->IsVideo()) {
            continue;
        }

        if (!firstVideoStream) {
            firstVideoStream = stream;
        }

        const bool streamIndexMatches = preferredStreamIndex >= 0 && stream->GetIndex() == preferredStreamIndex;
        const bool streamIdMatches = preferredStreamId >= 0 && stream->GetId() == preferredStreamId;

        if (preferredStreamIndex >= 0 && preferredStreamId >= 0) {
            if (streamIndexMatches && streamIdMatches) {
                result.stream = stream;
                result.streamIndex = stream->GetIndex();
                return result;
            }
            continue;
        }

        if ((preferredStreamIndex >= 0 && streamIndexMatches) || (preferredStreamId >= 0 && streamIdMatches)) {
            result.stream = stream;
            result.streamIndex = stream->GetIndex();
            return result;
        }
    }

    if (preferredStreamIndex < 0 && preferredStreamId < 0 && firstVideoStream) {
        result.stream = firstVideoStream;
        result.streamIndex = firstVideoStream->GetIndex();
        return result;
    }

    return result;
}

QImage convertFrameToImage(const FFmpegFunctions& ffmpeg, const AVFrameWrapper& frame)
{
    const int width = frame.GetWidth();
    const int height = frame.GetHeight();

    if (width <= 0 || height <= 0) {
        return {};
    }

    QImage image(width, height, QImage::Format_RGB32);
    if (image.isNull()) {
        return {};
    }

    std::array<const uint8_t*, 8> srcData {};
    std::array<int, 8> srcStride {};

    const int dataPointers = std::min<int>(frame.GetNumDataPointers(), static_cast<int>(srcData.size()));
    for (int i = 0; i < dataPointers; ++i) {
        srcData[i] = frame.GetData(i);
        srcStride[i] = frame.GetLineSize(i);
    }

    std::array<uint8_t*, 4> dstData { image.bits(), nullptr, nullptr, nullptr };
    std::array<int, 4> dstStride { static_cast<int>(image.bytesPerLine()), 0, 0, 0 };

    SwsContext* swsContext = ffmpeg.sws_getCachedContext(
        nullptr,
        width,
        height,
        static_cast<AVPixelFormatFwd>(frame.GetFormat()),
        width,
        height,
        ffmpeg.GetBGRAPixelFormat(),
        AUDACITY_SWS_BILINEAR,
        nullptr,
        nullptr,
        nullptr);

    if (!swsContext) {
        return {};
    }

    const int scaledHeight = ffmpeg.sws_scale(
        swsContext,
        srcData.data(),
        srcStride.data(),
        0,
        height,
        dstData.data(),
        dstStride.data());

    ffmpeg.sws_freeContext(swsContext);

    return scaledHeight == height ? image : QImage();
}

std::unique_ptr<AVCodecContextWrapper> openDecoderContext(const FFmpegFunctions& ffmpeg,
                                                          const AVStreamWrapper& stream,
                                                          VideoPreviewState& state)
{
    std::unique_ptr<AVCodecWrapper> decoder = ffmpeg.CreateDecoder(stream.GetAVCodecID());
    if (!decoder) {
        state = VideoPreviewState::UnsupportedCodec;
        return {};
    }

    std::unique_ptr<AVCodecContextWrapper> codecContext = stream.GetAVCodecContext();
    if (!codecContext) {
        state = VideoPreviewState::UnsupportedCodec;
        return {};
    }

    if (codecContext->Open(decoder.get()) < 0) {
        AVCodecContext* rawContext = codecContext->GetWrappedValue();
        codecContext.reset();
        if (rawContext && ffmpeg.avcodec_free_context) {
            ffmpeg.avcodec_free_context(&rawContext);
        }

        state = VideoPreviewState::UnsupportedCodec;
        return {};
    }

    state = VideoPreviewState::Ready;
    return codecContext;
}

void releaseDecoderContext(const FFmpegFunctions& ffmpeg, std::unique_ptr<AVCodecContextWrapper>& codecContext)
{
    AVCodecContext* rawContext = codecContext ? codecContext->GetWrappedValue() : nullptr;
    codecContext.reset();
    if (rawContext && ffmpeg.avcodec_free_context) {
        ffmpeg.avcodec_free_context(&rawContext);
    }
}
}

VideoProbeResult FFmpegVideoDecoder::probe(const muse::io::path_t& path, int preferredStreamIndex, int preferredStreamId)
{
    auto ffmpeg = FFmpegFunctions::Load();
    if (!ffmpeg || !ffmpeg->SupportsVideoDecode()) {
        return { VideoPreviewState::NoFfmpeg };
    }

    OpenVideoContext context = openVideoContext(path, preferredStreamIndex, preferredStreamId);
    if (!context.formatContext) {
        return { VideoPreviewState::DecodingError };
    }

    if (!context.stream) {
        return { VideoPreviewState::Empty };
    }

    VideoPreviewState state = VideoPreviewState::Empty;
    std::unique_ptr<AVCodecContextWrapper> codecContext = openDecoderContext(*context.ffmpeg, *context.stream, state);
    releaseDecoderContext(*context.ffmpeg, codecContext);

    if (state != VideoPreviewState::Ready) {
        return { state };
    }

    return { VideoPreviewState::Ready, context.streamIndex, context.stream->GetId() };
}

VideoDecodeResult FFmpegVideoDecoder::decodeFrame(const muse::io::path_t& path, int streamIndex, int streamId, double sourceSeconds)
{
    OpenVideoContext context = openVideoContext(path, streamIndex, streamId);
    if (!context.ffmpeg || !context.ffmpeg->SupportsVideoDecode()) {
        return { VideoPreviewState::NoFfmpeg };
    }

    if (!context.formatContext || !context.stream) {
        return { VideoPreviewState::DecodingError };
    }

    VideoPreviewState state = VideoPreviewState::Empty;
    std::unique_ptr<AVCodecContextWrapper> codecContext = openDecoderContext(*context.ffmpeg, *context.stream, state);
    if (!codecContext) {
        return { state };
    }

    const AudacityAVRational timeBase = context.stream->GetTimeBase();
    const int64_t streamStart = streamStartTimestamp(*context.stream);
    if (timeBase.num != 0 && timeBase.den != 0) {
        const int64_t timestamp = streamStart + timestampFromSeconds(std::max(0.0, sourceSeconds), timeBase);
        context.ffmpeg->av_seek_frame(
            context.formatContext->GetWrappedValue(),
            context.streamIndex,
            timestamp,
            AUDACITY_AVSEEK_FLAG_BACKWARD);
    }

    int packetsDecoded = 0;

    while (packetsDecoded < MAX_PACKETS_TO_DECODE) {
        std::unique_ptr<AVPacketWrapper> packet = context.formatContext->ReadNextPacket();
        if (!packet) {
            break;
        }

        if (packet->GetStreamIndex() != context.streamIndex) {
            continue;
        }

        ++packetsDecoded;

        int ret = context.ffmpeg->avcodec_send_packet(codecContext->GetWrappedValue(), packet->GetWrappedValue());
        if (ret < 0) {
            releaseDecoderContext(*context.ffmpeg, codecContext);
            return { VideoPreviewState::DecodingError };
        }

        while (ret >= 0) {
            std::unique_ptr<AVFrameWrapper> frame = context.ffmpeg->CreateAVFrameWrapper();
            if (!frame) {
                releaseDecoderContext(*context.ffmpeg, codecContext);
                return { VideoPreviewState::DecodingError };
            }

            ret = context.ffmpeg->avcodec_receive_frame(codecContext->GetWrappedValue(), frame->GetWrappedValue());
            if (ret == AUDACITY_AVERROR(EAGAIN) || ret == AUDACITY_AVERROR_EOF) {
                break;
            }

            if (ret < 0) {
                releaseDecoderContext(*context.ffmpeg, codecContext);
                return { VideoPreviewState::DecodingError };
            }

            const double frameTime = sourceSecondsFromTimestamp(frame->GetBestEffortTimestamp(), streamStart, timeBase);
            if (frameTime + FRAME_TIME_TOLERANCE < sourceSeconds) {
                continue;
            }

            QImage image = convertFrameToImage(*context.ffmpeg, *frame);
            releaseDecoderContext(*context.ffmpeg, codecContext);

            return image.isNull()
                   ? VideoDecodeResult { VideoPreviewState::DecodingError }
                   : VideoDecodeResult { VideoPreviewState::Ready, image, frameTime };
        }
    }

    releaseDecoderContext(*context.ffmpeg, codecContext);

    return { VideoPreviewState::DecodingError };
}
