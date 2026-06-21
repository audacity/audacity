/*
* Audacity: A Digital Audio Editor
*/

#include "au3exporter.h"

#include <algorithm>
#include <array>
#include <cctype>
#include <cmath>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include <wx/filefn.h>

#include "framework/global/async/asyncable.h"

#include "au3-basic-ui/BasicUI.h"
#include "au3-import-export/ExportPluginRegistry.h"
#include "au3-import-export/ExportUtils.h"
#include "au3-mixer/MixerOptions.h"
#include "au3-tags/Tags.h"
#include "au3-track/Track.h"
#include "au3-wave-track/WaveTrack.h"
#include "au3-strings/TranslatableString.h"

#include "RegisterExportPlugins.h"

#include "au3wrap/au3types.h"
#include "au3wrap/internal/wxtypes_convert.h"
#include "importexport/export/exportutils.h"
#include "mod-ffmpeg/lib-ffmpeg-support/FFmpegFunctions.h"

#include "translation.h"

using namespace au::au3;
using namespace au::importexport;
using au::videopreview::VideoLink;
using au::videopreview::VideoSegment;

#if defined(__WXMSW__)
#define OSINPUT(X) ((X).mb_str() ? (char*)(const char*)(X).mb_str() : "")
#elif defined(__WXMAC__)
#define OSFILENAME(X) ((char*)(const char*)(X).fn_str())
#define OSINPUT(X) OSFILENAME(X)
#else
#define OSFILENAME(X) ((char*)(const char*)(X).mb_str())
#define OSINPUT(X) OSFILENAME(X)
#endif

namespace {
constexpr double EPS = 1e-7;
constexpr int VIDEO_TIME_BASE = 90000;

struct ExportFormatRef
{
    ExportPlugin* plugin = nullptr;
    int formatIndex = -1;

    bool isValid() const
    {
        return plugin && formatIndex >= 0;
    }
};

enum class VideoOutputFormat
{
    SameAsOriginal,
    Mp4,
    WebM,
    Av1
};

struct VideoEncodingOptions
{
    VideoOutputFormat format = VideoOutputFormat::SameAsOriginal;
    int targetBitRate = 6000000;

    bool encodeVideo() const
    {
        return format != VideoOutputFormat::SameAsOriginal;
    }
};

std::string lowerAscii(std::string value);

std::string formatDescription(const ExportPlugin& plugin, int formatIndex)
{
    return plugin.GetFormatInfo(formatIndex).description.msgid().toStdString();
}

ExportFormatRef findFormatByName(const std::string& format)
{
    for (auto [plugin, formatIndex] : ExportPluginRegistry::Get()) {
        if (formatDescription(*plugin, formatIndex) == format) {
            return { plugin, formatIndex };
        }
    }

    return {};
}

std::string lowerAscii(std::string value)
{
    std::transform(value.begin(), value.end(), value.begin(), [](unsigned char c) {
        return static_cast<char>(std::tolower(c));
    });
    return value;
}

bool isVideoContainerExtension(const wxString& extension)
{
    static constexpr std::array<std::string_view, 10> videoExtensions {
        "mp4", "m4v", "mov", "mkv", "webm", "avi", "mpg", "mpeg", "mts", "m2ts"
    };

    const std::string ext = lowerAscii(extension.ToStdString());
    return std::find(videoExtensions.begin(), videoExtensions.end(), ext) != videoExtensions.end();
}

VideoOutputFormat videoOutputFormatFromCode(const std::string& code)
{
    if (code == "mp4") {
        return VideoOutputFormat::Mp4;
    }

    if (code == "webm") {
        return VideoOutputFormat::WebM;
    }

    if (code == "av1") {
        return VideoOutputFormat::Av1;
    }

    return VideoOutputFormat::SameAsOriginal;
}

int videoTargetBitRateFromQualityCode(const std::string& code)
{
    if (code == "low") {
        return 2500000;
    }

    if (code == "high") {
        return 12000000;
    }

    return 6000000;
}

VideoEncodingOptions videoEncodingOptions(const IExporter::Options& options)
{
    VideoEncodingOptions result;

    if (options.count(IExporter::OptionKey::VideoFormat)) {
        result.format = videoOutputFormatFromCode(options.at(IExporter::OptionKey::VideoFormat).toString());
    }

    if (options.count(IExporter::OptionKey::VideoQuality)) {
        result.targetBitRate = videoTargetBitRateFromQualityCode(options.at(IExporter::OptionKey::VideoQuality).toString());
    }

    return result;
}

std::vector<const char*> videoEncoderCandidates(VideoOutputFormat format)
{
    switch (format) {
    case VideoOutputFormat::Mp4:
        return { "libx264", "h264" };
    case VideoOutputFormat::WebM:
        return { "libvpx-vp9", "vp9", "libvpx" };
    case VideoOutputFormat::Av1:
        return { "libaom-av1", "libsvtav1", "av1" };
    case VideoOutputFormat::SameAsOriginal:
        break;
    }

    return {};
}

std::unique_ptr<AVCodecWrapper> createVideoEncoder(FFmpegFunctions& ffmpeg, VideoOutputFormat format)
{
    if (!ffmpeg.av_codec_is_encoder) {
        return {};
    }

    for (const char* name : videoEncoderCandidates(format)) {
        std::unique_ptr<AVCodecWrapper> encoder = ffmpeg.CreateEncoder(name);
        if (encoder && !encoder->IsAudio() && ffmpeg.av_codec_is_encoder(encoder->GetWrappedValue())) {
            return encoder;
        }
    }

    return {};
}

wxString defaultAudioExtension(const ExportPlugin& plugin, int format)
{
    const FormatInfo formatInfo = plugin.GetFormatInfo(format);
    if (!formatInfo.extensions.empty()) {
        return formatInfo.extensions.front();
    }

    return {};
}

wxFileName makeTemporaryAudioFilename(const wxFileName& targetFilename, const ExportPlugin& plugin, int format)
{
    wxString tempPath = wxFileName::CreateTempFileName(wxT("audacity-video-audio"));
    if (!tempPath.empty()) {
        ::wxRemoveFile(tempPath);
    }

    wxFileName tempFilename(tempPath);
    wxString extension = defaultAudioExtension(plugin, format);
    if (extension.empty()) {
        extension = targetFilename.GetExt();
    }

    if (!extension.empty()) {
        tempFilename.SetExt(extension);
    }

    if (::wxFileExists(tempFilename.GetFullPath())) {
        ::wxRemoveFile(tempFilename.GetFullPath());
    }

    return tempFilename;
}

std::unique_ptr<AVFormatContextWrapper> openInputContext(const FFmpegFunctions& ffmpeg, const wxString& path)
{
    std::unique_ptr<AVFormatContextWrapper> context = ffmpeg.CreateAVFormatContext();
    if (!context) {
        return {};
    }

    const auto openResult = context->OpenInputContext(path, nullptr, AVDictionaryWrapper(ffmpeg));
    if (openResult != AVIOContextWrapper::OpenResult::Success) {
        return {};
    }

    return context;
}

void releaseCodecContext(const FFmpegFunctions& ffmpeg, std::unique_ptr<AVCodecContextWrapper>& codecContext)
{
    AVCodecContext* rawContext = codecContext ? codecContext->GetWrappedValue() : nullptr;
    codecContext.reset();
    if (rawContext && ffmpeg.avcodec_free_context) {
        ffmpeg.avcodec_free_context(&rawContext);
    }
}

std::unique_ptr<AVCodecContextWrapper> openVideoDecoderContext(const FFmpegFunctions& ffmpeg, const AVStreamWrapper& stream)
{
    std::unique_ptr<AVCodecWrapper> decoder = ffmpeg.CreateDecoder(stream.GetAVCodecID());
    if (!decoder) {
        return {};
    }

    std::unique_ptr<AVCodecContextWrapper> codecContext = stream.GetAVCodecContext();
    if (!codecContext) {
        return {};
    }

    if (codecContext->Open(decoder.get()) < 0) {
        releaseCodecContext(ffmpeg, codecContext);
        return {};
    }

    return codecContext;
}

const AVStreamWrapper* firstStreamOfType(const AVFormatContextWrapper& context, bool video)
{
    for (unsigned int i = 0; i < context.GetStreamsCount(); ++i) {
        const AVStreamWrapper* stream = context.GetStream(static_cast<int>(i));
        if (!stream) {
            continue;
        }

        if ((video && stream->IsVideo()) || (!video && stream->IsAudio())) {
            return stream;
        }
    }

    return nullptr;
}

const AVStreamWrapper* videoStreamForLink(const AVFormatContextWrapper& context, const VideoLink& link)
{
    const AVStreamWrapper* firstVideoStream = nullptr;

    for (unsigned int i = 0; i < context.GetStreamsCount(); ++i) {
        const AVStreamWrapper* stream = context.GetStream(static_cast<int>(i));
        if (!stream || !stream->IsVideo()) {
            continue;
        }

        if (!firstVideoStream) {
            firstVideoStream = stream;
        }

        const bool streamIndexMatches = link.streamIndex >= 0 && stream->GetIndex() == link.streamIndex;
        const bool streamIdMatches = link.streamId >= 0 && stream->GetId() == link.streamId;

        if (link.streamIndex >= 0 && link.streamId >= 0) {
            if (streamIndexMatches && streamIdMatches) {
                return stream;
            }
            continue;
        }

        if ((link.streamIndex >= 0 && streamIndexMatches) || (link.streamId >= 0 && streamIdMatches)) {
            return stream;
        }
    }

    return firstVideoStream;
}

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

    return static_cast<int64_t>(std::llround(seconds * static_cast<double>(timeBase.den) / static_cast<double>(timeBase.num)));
}

int64_t streamStartTimestamp(const AVStreamWrapper& stream)
{
    const int64_t startTime = stream.GetStartTime();
    return startTime == AUDACITY_AV_NOPTS_VALUE ? 0 : startTime;
}

double sourceSecondsFromTimestamp(int64_t timestamp, const AVStreamWrapper& stream)
{
    return secondsFromTimestamp(timestamp - streamStartTimestamp(stream), stream.GetTimeBase());
}

double duration(double start, double end)
{
    return std::max(0.0, end - start);
}

double segmentScale(const VideoSegment& segment)
{
    const double sourceDuration = duration(segment.sourceStart, segment.sourceEnd);
    if (sourceDuration <= EPS) {
        return 1.0;
    }

    return duration(segment.projectStart, segment.projectEnd) / sourceDuration;
}

double projectSecondsFromSourceSeconds(const VideoSegment& segment, double sourceSeconds)
{
    return segment.projectStart + ((sourceSeconds - segment.sourceStart) * segmentScale(segment));
}

std::vector<VideoSegment> sortedValidSegments(const VideoLink& link)
{
    std::vector<VideoSegment> segments;
    segments.reserve(link.segments.size());
    for (const VideoSegment& segment : link.segments) {
        if (segment.isValid()) {
            segments.push_back(segment);
        }
    }

    std::sort(segments.begin(), segments.end(), [](const VideoSegment& left, const VideoSegment& right) {
        return left.projectStart < right.projectStart;
    });
    return segments;
}

double videoEndTime(const std::vector<VideoSegment>& segments)
{
    double endTime = 0.0;
    for (const VideoSegment& segment : segments) {
        endTime = std::max(endTime, segment.projectEnd);
    }
    return endTime;
}

bool copyStream(const AVStreamWrapper& source, AVStreamWrapper& destination)
{
    if (destination.CopyParametersFrom(source) < 0) {
        return false;
    }

    destination.SetTimeBase(source.GetTimeBase());
    destination.SetStartTime(source.GetStartTime());
    destination.SetDuration(source.GetDuration());
    destination.SetFramesCount(source.GetFramesCount());
    destination.SetDisposition(source.GetDisposition());
    destination.SetSampleAspectRatio(source.GetSampleAspectRatio());
    destination.SetMetadata(source.GetMetadata());
    return true;
}

bool writeAllPackets(FFmpegFunctions& ffmpeg, AVFormatContextWrapper& inputContext, const AVStreamWrapper& inputStream,
                     AVFormatContextWrapper& outputContext, const AVStreamWrapper& outputStream)
{
    const int inputStreamIndex = inputStream.GetIndex();
    const int outputStreamIndex = outputStream.GetIndex();
    const AudacityAVRational inputTimeBase = inputStream.GetTimeBase();
    const AudacityAVRational outputTimeBase = outputStream.GetTimeBase();

    while (std::unique_ptr<AVPacketWrapper> packet = inputContext.ReadNextPacket()) {
        if (packet->GetStreamIndex() != inputStreamIndex) {
            continue;
        }

        packet->SetStreamIndex(outputStreamIndex);
        packet->RescalePresentationTimestamp(inputTimeBase, outputTimeBase);
        packet->RescaleDecompressionTimestamp(inputTimeBase, outputTimeBase);
        packet->RescaleDuration(inputTimeBase, outputTimeBase);
        packet->SetPos(-1);

        if (ffmpeg.av_interleaved_write_frame(outputContext.GetWrappedValue(), packet->GetWrappedValue()) < 0) {
            return false;
        }
    }

    return true;
}

std::optional<double> packetSourceSeconds(const AVPacketWrapper& packet, const AVStreamWrapper& inputStream)
{
    int64_t timestamp = packet.GetPresentationTimestamp();
    if (timestamp == AUDACITY_AV_NOPTS_VALUE) {
        timestamp = packet.GetDecompressionTimestamp();
    }

    if (timestamp == AUDACITY_AV_NOPTS_VALUE) {
        return std::nullopt;
    }

    return sourceSecondsFromTimestamp(timestamp, inputStream);
}

int64_t mappedTimestamp(const VideoSegment& segment, const AVStreamWrapper& inputStream, const AVStreamWrapper& outputStream,
                        int64_t inputTimestamp)
{
    if (inputTimestamp == AUDACITY_AV_NOPTS_VALUE) {
        return AUDACITY_AV_NOPTS_VALUE;
    }

    double projectSeconds = projectSecondsFromSourceSeconds(segment, sourceSecondsFromTimestamp(inputTimestamp, inputStream));
    if (projectSeconds < 0.0) {
        projectSeconds = 0.0;
    }

    return timestampFromSeconds(projectSeconds, outputStream.GetTimeBase());
}

int mappedDuration(const VideoSegment& segment, const AVStreamWrapper& inputStream, const AVStreamWrapper& outputStream, int inputDuration)
{
    if (inputDuration <= 0) {
        return inputDuration;
    }

    const double sourceDurationSeconds = secondsFromTimestamp(inputDuration, inputStream.GetTimeBase());
    const double projectDurationSeconds = sourceDurationSeconds * segmentScale(segment);
    return static_cast<int>(std::max<int64_t>(0, timestampFromSeconds(projectDurationSeconds, outputStream.GetTimeBase())));
}

bool writeVideoSegmentPackets(FFmpegFunctions& ffmpeg, AVFormatContextWrapper& inputContext, const AVStreamWrapper& inputStream,
                              AVFormatContextWrapper& outputContext, const AVStreamWrapper& outputStream,
                              const VideoSegment& segment)
{
    const int inputStreamIndex = inputStream.GetIndex();
    const int outputStreamIndex = outputStream.GetIndex();

    while (std::unique_ptr<AVPacketWrapper> packet = inputContext.ReadNextPacket()) {
        if (packet->GetStreamIndex() != inputStreamIndex) {
            continue;
        }

        const std::optional<double> packetSeconds = packetSourceSeconds(*packet, inputStream);
        if (!packetSeconds.has_value()) {
            continue;
        }

        if (*packetSeconds < segment.sourceStart - EPS || *packetSeconds >= segment.sourceEnd - EPS) {
            continue;
        }

        packet->SetStreamIndex(outputStreamIndex);
        packet->SetPresentationTimestamp(mappedTimestamp(segment, inputStream, outputStream, packet->GetPresentationTimestamp()));
        packet->SetDecompressionTimestamp(mappedTimestamp(segment, inputStream, outputStream, packet->GetDecompressionTimestamp()));
        packet->SetDuration(mappedDuration(segment, inputStream, outputStream, packet->GetDuration()));
        packet->SetPos(-1);

        if (ffmpeg.av_interleaved_write_frame(outputContext.GetWrappedValue(), packet->GetWrappedValue()) < 0) {
            return false;
        }
    }

    return true;
}

bool writeVideoPackets(FFmpegFunctions& ffmpeg, const wxFileName& sourceVideoFilename, const VideoLink& link,
                       AVFormatContextWrapper& outputContext, const AVStreamWrapper& outputStream)
{
    const std::vector<VideoSegment> segments = sortedValidSegments(link);
    if (segments.empty()) {
        return false;
    }

    for (const VideoSegment& segment : segments) {
        std::unique_ptr<AVFormatContextWrapper> inputContext = openInputContext(ffmpeg, sourceVideoFilename.GetFullPath());
        if (!inputContext) {
            return false;
        }

        const AVStreamWrapper* inputStream = videoStreamForLink(*inputContext, link);
        if (!inputStream) {
            return false;
        }

        if (!writeVideoSegmentPackets(ffmpeg, *inputContext, *inputStream, outputContext, outputStream, segment)) {
            return false;
        }
    }

    return true;
}

bool receiveEncodedPackets(FFmpegFunctions& ffmpeg, AVCodecContextWrapper& encoderContext,
                           AVFormatContextWrapper& outputContext, const AVStreamWrapper& outputStream)
{
    while (true) {
        std::unique_ptr<AVPacketWrapper> packet = ffmpeg.CreateAVPacketWrapper();
        if (!packet) {
            return false;
        }

        const int ret = ffmpeg.avcodec_receive_packet(encoderContext.GetWrappedValue(), packet->GetWrappedValue());
        if (ret == AUDACITY_AVERROR(EAGAIN) || ret == AUDACITY_AVERROR_EOF) {
            return true;
        }

        if (ret < 0) {
            return false;
        }

        packet->SetStreamIndex(outputStream.GetIndex());
        packet->RescalePresentationTimestamp(encoderContext.GetTimeBase(), outputStream.GetTimeBase());
        packet->RescaleDecompressionTimestamp(encoderContext.GetTimeBase(), outputStream.GetTimeBase());
        packet->RescaleDuration(encoderContext.GetTimeBase(), outputStream.GetTimeBase());
        packet->SetPos(-1);

        if (ffmpeg.av_interleaved_write_frame(outputContext.GetWrappedValue(), packet->GetWrappedValue()) < 0) {
            return false;
        }
    }
}

bool sendFrameToEncoder(FFmpegFunctions& ffmpeg, AVCodecContextWrapper& encoderContext,
                        AVFormatContextWrapper& outputContext, const AVStreamWrapper& outputStream,
                        AVFrameWrapper* frame)
{
    const int ret = ffmpeg.avcodec_send_frame(encoderContext.GetWrappedValue(), frame ? frame->GetWrappedValue() : nullptr);
    return ret >= 0 && receiveEncodedPackets(ffmpeg, encoderContext, outputContext, outputStream);
}

bool encodeDecodedFrame(FFmpegFunctions& ffmpeg, const AVStreamWrapper& inputStream, AVCodecContextWrapper& encoderContext,
                        AVFormatContextWrapper& outputContext, const AVStreamWrapper& outputStream,
                        const VideoSegment& segment, const AVFrameWrapper& decodedFrame)
{
    const double sourceSeconds = sourceSecondsFromTimestamp(decodedFrame.GetBestEffortTimestamp(), inputStream);
    if (sourceSeconds < segment.sourceStart - EPS || sourceSeconds >= segment.sourceEnd - EPS) {
        return true;
    }

    const int width = encoderContext.GetWidth();
    const int height = encoderContext.GetHeight();
    if (width <= 0 || height <= 0) {
        return false;
    }

    std::unique_ptr<AVFrameWrapper> encodedFrame = ffmpeg.CreateAVFrameWrapper();
    if (!encodedFrame) {
        return false;
    }

    encodedFrame->SetFormat(encoderContext.GetPixelFormat());
    encodedFrame->SetWidth(width);
    encodedFrame->SetHeight(height);
    encodedFrame->SetPresentationTimestamp(timestampFromSeconds(projectSecondsFromSourceSeconds(segment, sourceSeconds),
                                                                encoderContext.GetTimeBase()));

    if (encodedFrame->GetBuffer(32) < 0) {
        return false;
    }

    std::array<const uint8_t*, 8> srcData {};
    std::array<int, 8> srcStride {};
    const int dataPointers = std::min<int>(decodedFrame.GetNumDataPointers(), static_cast<int>(srcData.size()));
    for (int i = 0; i < dataPointers; ++i) {
        srcData[i] = decodedFrame.GetData(i);
        srcStride[i] = decodedFrame.GetLineSize(i);
    }

    std::array<uint8_t*, 8> dstData {};
    std::array<int, 8> dstStride {};
    const int dstPointers = std::min<int>(encodedFrame->GetNumDataPointers(), static_cast<int>(dstData.size()));
    for (int i = 0; i < dstPointers; ++i) {
        dstData[i] = encodedFrame->GetData(i);
        dstStride[i] = encodedFrame->GetLineSize(i);
    }

    SwsContext* swsContext = ffmpeg.sws_getCachedContext(
        nullptr,
        decodedFrame.GetWidth(),
        decodedFrame.GetHeight(),
        static_cast<AVPixelFormatFwd>(decodedFrame.GetFormat()),
        width,
        height,
        encoderContext.GetPixelFormat(),
        AUDACITY_SWS_BILINEAR,
        nullptr,
        nullptr,
        nullptr);
    if (!swsContext) {
        return false;
    }

    const int scaledHeight = ffmpeg.sws_scale(
        swsContext,
        srcData.data(),
        srcStride.data(),
        0,
        decodedFrame.GetHeight(),
        dstData.data(),
        dstStride.data());

    ffmpeg.sws_freeContext(swsContext);

    if (scaledHeight != height) {
        return false;
    }

    return sendFrameToEncoder(ffmpeg, encoderContext, outputContext, outputStream, encodedFrame.get());
}

bool receiveDecodedFrames(FFmpegFunctions& ffmpeg, AVCodecContextWrapper& decoderContext, const AVStreamWrapper& inputStream,
                          AVCodecContextWrapper& encoderContext, AVFormatContextWrapper& outputContext,
                          const AVStreamWrapper& outputStream, const VideoSegment& segment)
{
    while (true) {
        std::unique_ptr<AVFrameWrapper> frame = ffmpeg.CreateAVFrameWrapper();
        if (!frame) {
            return false;
        }

        const int ret = ffmpeg.avcodec_receive_frame(decoderContext.GetWrappedValue(), frame->GetWrappedValue());
        if (ret == AUDACITY_AVERROR(EAGAIN) || ret == AUDACITY_AVERROR_EOF) {
            return true;
        }

        if (ret < 0) {
            return false;
        }

        if (!encodeDecodedFrame(ffmpeg, inputStream, encoderContext, outputContext, outputStream, segment, *frame)) {
            return false;
        }
    }
}

bool writeEncodedVideoSegment(FFmpegFunctions& ffmpeg, const wxFileName& sourceVideoFilename, const VideoLink& link,
                              AVCodecContextWrapper& encoderContext, AVFormatContextWrapper& outputContext,
                              const AVStreamWrapper& outputStream, const VideoSegment& segment)
{
    std::unique_ptr<AVFormatContextWrapper> inputContext = openInputContext(ffmpeg, sourceVideoFilename.GetFullPath());
    if (!inputContext) {
        return false;
    }

    const AVStreamWrapper* inputStream = videoStreamForLink(*inputContext, link);
    if (!inputStream) {
        return false;
    }

    std::unique_ptr<AVCodecContextWrapper> decoderContext = openVideoDecoderContext(ffmpeg, *inputStream);
    if (!decoderContext) {
        return false;
    }

    const int64_t seekTimestamp = streamStartTimestamp(*inputStream)
                                  + timestampFromSeconds(std::max(0.0, segment.sourceStart), inputStream->GetTimeBase());
    ffmpeg.av_seek_frame(inputContext->GetWrappedValue(), inputStream->GetIndex(), seekTimestamp, AUDACITY_AVSEEK_FLAG_BACKWARD);

    bool ok = true;
    while (ok) {
        std::unique_ptr<AVPacketWrapper> packet = inputContext->ReadNextPacket();
        if (!packet) {
            break;
        }

        if (packet->GetStreamIndex() != inputStream->GetIndex()) {
            continue;
        }

        const std::optional<double> packetSeconds = packetSourceSeconds(*packet, *inputStream);
        if (packetSeconds.has_value() && *packetSeconds > segment.sourceEnd + 1.0) {
            break;
        }

        int ret = ffmpeg.avcodec_send_packet(decoderContext->GetWrappedValue(), packet->GetWrappedValue());
        if (ret < 0) {
            ok = false;
            break;
        }

        ok = receiveDecodedFrames(ffmpeg, *decoderContext, *inputStream, encoderContext, outputContext, outputStream, segment);
    }

    if (ok) {
        ok = ffmpeg.avcodec_send_packet(decoderContext->GetWrappedValue(), nullptr) >= 0
             && receiveDecodedFrames(ffmpeg, *decoderContext, *inputStream, encoderContext, outputContext, outputStream, segment);
    }

    releaseCodecContext(ffmpeg, decoderContext);
    return ok;
}

bool writeEncodedVideoPackets(FFmpegFunctions& ffmpeg, const wxFileName& sourceVideoFilename, const VideoLink& link,
                              AVCodecContextWrapper& encoderContext, AVFormatContextWrapper& outputContext,
                              const AVStreamWrapper& outputStream)
{
    const std::vector<VideoSegment> segments = sortedValidSegments(link);
    if (segments.empty()) {
        return false;
    }

    for (const VideoSegment& segment : segments) {
        if (!writeEncodedVideoSegment(ffmpeg, sourceVideoFilename, link, encoderContext, outputContext, outputStream, segment)) {
            return false;
        }
    }

    return sendFrameToEncoder(ffmpeg, encoderContext, outputContext, outputStream, nullptr);
}

std::string remuxLinkedVideo(const VideoLink& link, const wxFileName& sourceVideoFilename, const wxFileName& audioFilename,
                             const wxFileName& targetFilename)
{
    std::shared_ptr<FFmpegFunctions> ffmpeg = FFmpegFunctions::Load();
    if (!ffmpeg || ffmpeg->AVFormatVersion.Major < 59) {
        return muse::trc("export", "FFmpeg 5 or newer is required to export linked video.");
    }

    std::unique_ptr<AVFormatContextWrapper> videoInput = openInputContext(*ffmpeg, sourceVideoFilename.GetFullPath());
    if (!videoInput) {
        return muse::trc("export", "Could not open linked video source for export.");
    }

    std::unique_ptr<AVFormatContextWrapper> audioInput = openInputContext(*ffmpeg, audioFilename.GetFullPath());
    if (!audioInput) {
        return muse::trc("export", "Could not open exported audio for video muxing.");
    }

    const std::vector<VideoSegment> segments = sortedValidSegments(link);
    if (segments.empty()) {
        return muse::trc("export", "Could not find video segments to export.");
    }

    const AVStreamWrapper* videoInputStream = videoStreamForLink(*videoInput, link);
    const AVStreamWrapper* audioInputStream = firstStreamOfType(*audioInput, false);
    if (!videoInputStream || !audioInputStream) {
        return muse::trc("export", "Could not find video and audio streams for export.");
    }

    const wxString targetPath = targetFilename.GetFullPath();
    std::unique_ptr<AVOutputFormatWrapper> outputFormat = ffmpeg->GuessOutputFormat(nullptr, OSINPUT(targetPath), nullptr);
    if (!outputFormat) {
        return muse::trc("export", "Could not determine the video export container.");
    }
    const int outputFormatFlags = outputFormat->GetFlags();

    std::unique_ptr<AVFormatContextWrapper> outputContext = ffmpeg->CreateAVFormatContext();
    if (!outputContext) {
        return muse::trc("export", "Could not allocate the video export context.");
    }
    outputContext->SetOutputFormat(std::move(outputFormat));
    outputContext->SetFilename(OSINPUT(targetPath));

    std::unique_ptr<AVStreamWrapper> videoOutputStream = outputContext->CreateStream();
    std::unique_ptr<AVStreamWrapper> audioOutputStream = outputContext->CreateStream();
    if (!videoOutputStream || !audioOutputStream) {
        return muse::trc("export", "Could not create video export streams.");
    }

    if (!copyStream(*videoInputStream, *videoOutputStream) || !copyStream(*audioInputStream, *audioOutputStream)) {
        return muse::trc("export", "Could not copy stream parameters for video export.");
    }
    videoOutputStream->SetStartTime(0);
    videoOutputStream->SetDuration(timestampFromSeconds(videoEndTime(segments), videoOutputStream->GetTimeBase()));

    if (!(outputFormatFlags & AUDACITY_AVFMT_NOFILE)) {
        const auto openResult = outputContext->OpenOutputContext(targetPath);
        if (openResult != AVIOContextWrapper::OpenResult::Success) {
            return muse::trc("export", "Could not open video export file for writing.");
        }
    }

    if (ffmpeg->avformat_write_header(outputContext->GetWrappedValue(), nullptr) < 0) {
        return muse::trc("export", "Could not write the video export header.");
    }

    if (!writeVideoPackets(*ffmpeg, sourceVideoFilename, link, *outputContext, *videoOutputStream)
        || !writeAllPackets(*ffmpeg, *audioInput, *audioInputStream, *outputContext, *audioOutputStream)) {
        return muse::trc("export", "Could not write video export packets.");
    }

    if (ffmpeg->av_write_trailer(outputContext->GetWrappedValue()) < 0) {
        return muse::trc("export", "Could not finalize the video export file.");
    }

    return {};
}

std::string encodeLinkedVideo(const VideoLink& link, const wxFileName& sourceVideoFilename, const wxFileName& audioFilename,
                              const wxFileName& targetFilename, const VideoEncodingOptions& options)
{
    std::shared_ptr<FFmpegFunctions> ffmpeg = FFmpegFunctions::Load();
    if (!ffmpeg || ffmpeg->AVFormatVersion.Major < 59 || !ffmpeg->avcodec_send_packet || !ffmpeg->avcodec_receive_frame
        || !ffmpeg->avcodec_send_frame || !ffmpeg->avcodec_receive_packet || !ffmpeg->av_frame_get_buffer
        || !ffmpeg->sws_getCachedContext || !ffmpeg->sws_scale || !ffmpeg->sws_freeContext) {
        return muse::trc("export", "FFmpeg 5 or newer with video encoding support is required to export encoded video.");
    }

    std::unique_ptr<AVFormatContextWrapper> videoInput = openInputContext(*ffmpeg, sourceVideoFilename.GetFullPath());
    if (!videoInput) {
        return muse::trc("export", "Could not open linked video source for export.");
    }

    std::unique_ptr<AVFormatContextWrapper> audioInput = openInputContext(*ffmpeg, audioFilename.GetFullPath());
    if (!audioInput) {
        return muse::trc("export", "Could not open exported audio for video muxing.");
    }

    const std::vector<VideoSegment> segments = sortedValidSegments(link);
    if (segments.empty()) {
        return muse::trc("export", "Could not find video segments to export.");
    }

    const AVStreamWrapper* videoInputStream = videoStreamForLink(*videoInput, link);
    const AVStreamWrapper* audioInputStream = firstStreamOfType(*audioInput, false);
    if (!videoInputStream || !audioInputStream) {
        return muse::trc("export", "Could not find video and audio streams for export.");
    }

    std::unique_ptr<AVCodecContextWrapper> decoderContext = openVideoDecoderContext(*ffmpeg, *videoInputStream);
    if (!decoderContext) {
        return muse::trc("export", "Could not open linked video decoder for export.");
    }

    const int sourceWidth = decoderContext->GetWidth();
    const int sourceHeight = decoderContext->GetHeight();
    releaseCodecContext(*ffmpeg, decoderContext);
    if (sourceWidth <= 0 || sourceHeight <= 0) {
        return muse::trc("export", "Could not determine linked video dimensions.");
    }

    std::unique_ptr<AVCodecWrapper> encoder = createVideoEncoder(*ffmpeg, options.format);
    if (!encoder) {
        return muse::trc("export", "Could not find a video encoder for the selected video format.");
    }

    std::unique_ptr<AVCodecContextWrapper> encoderContext = ffmpeg->CreateAVCodecContextWrapperFromCodec(std::move(encoder));
    if (!encoderContext || !encoderContext->GetCodec()) {
        return muse::trc("export", "Could not allocate the video encoder context.");
    }

    const int outputWidth = std::max(2, sourceWidth - (sourceWidth % 2));
    const int outputHeight = std::max(2, sourceHeight - (sourceHeight % 2));
    encoderContext->SetWidth(outputWidth);
    encoderContext->SetHeight(outputHeight);
    encoderContext->SetPixelFormat(ffmpeg->GetYUV420PPixelFormat());
    encoderContext->SetTimeBase({ 1, VIDEO_TIME_BASE });
    encoderContext->SetFrameRate({ 30, 1 });
    encoderContext->SetBitRate(options.targetBitRate);
    encoderContext->SetCodecTag(0);

    const wxString targetPath = targetFilename.GetFullPath();
    std::unique_ptr<AVOutputFormatWrapper> outputFormat = ffmpeg->GuessOutputFormat(nullptr, OSINPUT(targetPath), nullptr);
    if (!outputFormat) {
        return muse::trc("export", "Could not determine the video export container.");
    }
    const int outputFormatFlags = outputFormat->GetFlags();
    if (outputFormatFlags & AUDACITY_AVFMT_GLOBALHEADER) {
        encoderContext->SetFlags(encoderContext->GetFlags() | AUDACITY_AV_CODEC_FLAG_GLOBAL_HEADER);
    }

    if (encoderContext->Open(encoderContext->GetCodec()) < 0) {
        return muse::trc("export", "Could not open the selected video encoder.");
    }

    std::unique_ptr<AVFormatContextWrapper> outputContext = ffmpeg->CreateAVFormatContext();
    if (!outputContext) {
        return muse::trc("export", "Could not allocate the video export context.");
    }
    outputContext->SetOutputFormat(std::move(outputFormat));
    outputContext->SetFilename(OSINPUT(targetPath));

    std::unique_ptr<AVStreamWrapper> videoOutputStream = outputContext->CreateStream();
    std::unique_ptr<AVStreamWrapper> audioOutputStream = outputContext->CreateStream();
    if (!videoOutputStream || !audioOutputStream) {
        return muse::trc("export", "Could not create video export streams.");
    }

    videoOutputStream->SetTimeBase(encoderContext->GetTimeBase());
    videoOutputStream->SetStartTime(0);
    videoOutputStream->SetDuration(timestampFromSeconds(videoEndTime(segments), videoOutputStream->GetTimeBase()));
    if (videoOutputStream->SetParametersFromContext(*encoderContext) < 0 || !copyStream(*audioInputStream, *audioOutputStream)) {
        return muse::trc("export", "Could not copy stream parameters for video export.");
    }

    if (!(outputFormatFlags & AUDACITY_AVFMT_NOFILE)) {
        const auto openResult = outputContext->OpenOutputContext(targetPath);
        if (openResult != AVIOContextWrapper::OpenResult::Success) {
            return muse::trc("export", "Could not open video export file for writing.");
        }
    }

    if (ffmpeg->avformat_write_header(outputContext->GetWrappedValue(), nullptr) < 0) {
        return muse::trc("export", "Could not write the video export header.");
    }

    if (!writeEncodedVideoPackets(*ffmpeg, sourceVideoFilename, link, *encoderContext, *outputContext, *videoOutputStream)
        || !writeAllPackets(*ffmpeg, *audioInput, *audioInputStream, *outputContext, *audioOutputStream)) {
        return muse::trc("export", "Could not write video export packets.");
    }

    if (ffmpeg->av_write_trailer(outputContext->GetWrappedValue()) < 0) {
        return muse::trc("export", "Could not finalize the video export file.");
    }

    return {};
}

struct LinkedVideoExport
{
    bool enabled = false;
    VideoLink link;
    wxFileName sourceVideoFilename;
    wxFileName audioFilename;
    wxFileName targetFilename;
    VideoEncodingOptions encodingOptions;
};

LinkedVideoExport linkedVideoExport(const au::videopreview::IVideoPreviewService* videoPreviewService,
                                    const ExportPlugin& plugin, int format, ExportProcessType processType,
                                    const wxFileName& targetFilename, VideoEncodingOptions encodingOptions)
{
    LinkedVideoExport result;
    if (!videoPreviewService || processType != ExportProcessType::FULL_PROJECT_AUDIO_AND_VIDEO
        || !isVideoContainerExtension(targetFilename.GetExt())) {
        return result;
    }

    VideoLink link;
    for (const VideoLink& candidate : videoPreviewService->links()) {
        if (candidate.isValid()) {
            link = candidate;
            break;
        }
    }

    if (!link.isValid()) {
        return result;
    }

    wxFileName sourceVideoFilename = wxFromString(link.sourcePath.toString());
    if (!sourceVideoFilename.FileExists()) {
        return result;
    }

    result.enabled = true;
    result.link = link;
    result.sourceVideoFilename = std::move(sourceVideoFilename);
    result.audioFilename = makeTemporaryAudioFilename(targetFilename, plugin, format);
    result.targetFilename = targetFilename;
    result.encodingOptions = encodingOptions;
    return result;
}
}

class ProgressDelegate : public ExportProcessorDelegate, public muse::async::Asyncable
{
    muse::ProgressPtr m_progress;
    std::atomic<bool> m_cancelled { false };
public:
    ProgressDelegate(muse::ProgressPtr progress)
        : m_progress(progress)
    {
        m_progress->canceled().onNotify(this, [this] { m_cancelled = true; });
    }

    bool IsCancelled() const override { return m_cancelled; }
    bool IsStopped()   const override { return false; }
    void SetStatusString(const ::TranslatableString&) override {}
    void OnProgress(double) override {}
};

class DialogExportProgressDelegate : public ExportProcessorDelegate
{
    std::atomic<bool> mCancelled { false };
    std::atomic<bool> mStopped { false };
    std::atomic<double> mProgress {};

    TranslatableString mStatus;

    std::unique_ptr<BasicUI::ProgressDialog> mProgressDialog;
public:

    bool IsCancelled() const override
    {
        return mCancelled;
    }

    bool IsStopped() const override
    {
        return mStopped;
    }

    void SetStatusString(const ::TranslatableString& str) override
    {
        mStatus = str;
    }

    void OnProgress(double progress) override
    {
        mProgress = progress;
    }

    void UpdateUI()
    {
        constexpr long long ProgressSteps = 1000ul;

        if (!mProgressDialog) {
            mProgressDialog = BasicUI::MakeProgress(::TranslatableString("import-export", "Export"), mStatus);
        } else {
            mProgressDialog->SetMessage(mStatus);
        }

        const auto result = mProgressDialog->Poll(mProgress * ProgressSteps, ProgressSteps);

        if (result == BasicUI::ProgressResult::Cancelled) {
            if (!mStopped) {
                mCancelled = true;
            }
        } else if (result == BasicUI::ProgressResult::Stopped) {
            if (!mCancelled) {
                mStopped = true;
            }
        }
    }
};

void Au3Exporter::init()
{
    RegisterExportPlugins();
    ExportPluginRegistry::Get().Initialize();
}

muse::Ret Au3Exporter::exportData(const muse::io::path_t& path, const Options& options, muse::ProgressPtr progress)
{
    const std::string requestedFormatName = options.count(OptionKey::Format)
                                            ? options.at(OptionKey::Format).toString()
                                            : exportConfiguration()->currentFormat();
    const ExportFormatRef requestedFormat = findFormatByName(requestedFormatName);
    if (!requestedFormat.isValid()) {
        return muse::make_ret(muse::Ret::Code::InternalError,
                              muse::trc("export", "Could not find the requested export format."));
    }

    const ExportProcessType processType = options.count(OptionKey::ProcessType)
                                          ? options.at(OptionKey::ProcessType).toEnum<ExportProcessType>()
                                          : exportConfiguration()->processType();

    const int exportChannelsType = options.count(OptionKey::ExportChannelsType)
                                   ? options.at(OptionKey::ExportChannelsType).toInt()
                                   : exportConfiguration()->exportChannelsType();

    const int exportChannels = options.count(OptionKey::ExportChannels)
                               ? options.at(OptionKey::ExportChannels).toInt()
                               : exportConfiguration()->exportChannels();

    const muse::Val exportCustomChannelMapping = options.count(OptionKey::ExportCustomChannelMapping)
                                                 ? options.at(OptionKey::ExportCustomChannelMapping)
                                                 : exportConfiguration()->exportCustomChannelMapping();

    const int exportSampleRate = options.count(OptionKey::ExportSampleRate)
                                 ? options.at(OptionKey::ExportSampleRate).toInt()
                                 : exportConfiguration()->exportSampleRate();

    ExportParameters parameters;
    if (options.count(OptionKey::Parameters)) {
        for (const auto& entryVal : options.at(OptionKey::Parameters).toList()) {
            const auto& m = entryVal.toMap();
            const int id = m.at("id").toInt();
            const auto& valueVal = m.at("value");
            OptionValue value;
            switch (valueVal.type()) {
            case muse::Val::Type::Bool:
                value = valueVal.toBool();
                break;
            case muse::Val::Type::Int:
                value = valueVal.toInt();
                break;
            case muse::Val::Type::Double:
                value = valueVal.toDouble();
                break;
            case muse::Val::Type::String:
                value = valueVal.toString();
                break;
            default: break;
            }
            parameters.emplace_back(id, value);
        }
    } else {
        if (requestedFormat.plugin) {
            auto editor = requestedFormat.plugin->CreateOptionsEditor(requestedFormat.formatIndex, nullptr);
            editor->Load(*gPrefs);
            for (const auto& [id, val] : ExportUtils::ParametersFromEditor(*editor)) {
                parameters.emplace_back(id, std::visit([](auto v) -> OptionValue { return v; }, val));
            }
        }
    }

    wxFileName wxfilename = wxFromString(path.toString());

    Au3Project* project = reinterpret_cast<Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
    IF_ASSERT_FAILED(project) {
        return muse::make_ret(muse::Ret::Code::InternalError);
    }

    int formatIdx = requestedFormat.formatIndex;
    if (formatIdx == -1) {
        return muse::make_ret(muse::Ret::Code::InternalError);
    }
    m_format = formatIdx;

    m_plugin = requestedFormat.plugin;
    if (!m_plugin) {
        return muse::make_ret(muse::Ret::Code::InternalError);
    }

    const VideoEncodingOptions encodingOptions = videoEncodingOptions(options);
    const LinkedVideoExport videoExport = linkedVideoExport(videoPreviewService().get(), *m_plugin, formatIdx, processType, wxfilename,
                                                           encodingOptions);
    if (processType == ExportProcessType::FULL_PROJECT_AUDIO_AND_VIDEO && !videoExport.enabled) {
        return muse::make_ret(muse::Ret::Code::InternalError,
                              muse::trc("export", "No linked video is available for video export."));
    }
    const wxFileName actualFilename = videoExport.enabled ? videoExport.audioFilename : wxfilename;

    m_parameters.clear();
    for (const auto& [id, val] : parameters) {
        m_parameters.emplace_back(id, std::visit([](auto v) -> ExportValue { return v; }, val));
    }

    m_selectedOnly = false;
    // TODO: implement other ExportProcessType's selections
    if (processType == ExportProcessType::SELECTED_AUDIO) {
        m_t0
            = !selectionController()->timeSelectionIsEmpty() ? selectionController()->dataSelectedStartTime()
              : selectionController()->leftMostSelectedClipStartTime().value_or(0.0);
        m_t1
            = !selectionController()->timeSelectionIsEmpty() ? selectionController()->dataSelectedEndTime()
              : selectionController()->rightMostSelectedClipEndTime().value_or(0.0);
        m_selectedOnly = true;
    } else if (processType == ExportProcessType::AUDIO_IN_LOOP_REGION) {
        auto region = playbackController()->loopRegion();
        m_t0 = region.start;
        m_t1 = region.end;
    } else {
        auto trackeditProject = globalContext()->currentProject()->trackeditProject();

        m_t0 = 0.0;
        m_t1 = trackeditProject->totalTime().to_double();
    }

    m_tags = &Tags::Get(*project);

    auto exportedTracks = ExportUtils::FindExportWaveTracks(TrackList::Get(*project), m_selectedOnly);
    if (exportedTracks.empty()) {
        //! NOTE: All selected audio is muted
        return muse::make_ret(muse::Ret::Code::InternalError, muse::trc("export", "All selected audio is muted"));
    }

    int inputChannelsCount = 0;
    for (const auto& exportedTrack : exportedTracks) {
        inputChannelsCount += exportedTrack->NChannels();
    }

    auto downMix = std::make_unique<MixerOptions::Downmix>(inputChannelsCount, exportChannels);
    if (ExportChannelsPref::ExportChannels(exportChannelsType) == ExportChannelsPref::ExportChannels::MONO) {
        m_numChannels = 1;
    } else if (ExportChannelsPref::ExportChannels(exportChannelsType)
               == ExportChannelsPref::ExportChannels::STEREO) {
        m_numChannels = 2;
    } else {
        //Figure out the final channel mapping: mixer dialog shows
        //all tracks regardless of their mute/solo state, but
        //muted channels should not be present in exported file -
        //apply channel mask to exclude them
        auto channelMask = prepareChannelMask();
        downMix = std::make_unique<MixerOptions::Downmix>(*downMix, channelMask);
        m_mixerSpec = downMix.get();

        const std::vector<std::vector<bool> > matrix = utils::valToMatrix(exportCustomChannelMapping);
        m_numChannels = exportChannels;

        for (int in = 0; in < inputChannelsCount; ++in) {
            for (unsigned int out = 0; out < m_numChannels; ++out) {
                m_mixerSpec->mMap[in][out] = false;
            }
        }

        const int rows = std::min(inputChannelsCount, static_cast<int>(matrix.size()));
        for (int in = 0; in < rows; ++in) {
            const int cols = std::min(static_cast<int>(m_numChannels), static_cast<int>(matrix[in].size()));
            for (int out = 0; out < cols; ++out) {
                if (matrix[in][out]) {
                    m_mixerSpec->mMap[in][out] = true;
                }
            }
        }
    }

    m_sampleRate = exportSampleRate;

    try {
        auto processor = m_plugin->CreateProcessor(m_format);
        if (!processor->Initialize(*project,
                                   m_parameters,
                                   actualFilename.GetFullPath(),
                                   m_t0, m_t1, m_selectedOnly,
                                   m_sampleRate, m_numChannels,
                                   m_mixerSpec,
                                   m_tags)) {
            return muse::make_ret(muse::Ret::Code::InternalError);
        }

        auto exportTask = ExportTask([actualFilename,
                                      targetFilename = wxfilename,
                                      videoExportEnabled = videoExport.enabled,
                                      processor = std::shared_ptr<ExportProcessor>(processor.release())]
                                     (ExportProcessorDelegate& delegate)
        {
            auto result = ExportResult::Error;
            auto cleanup = finally([&] {
                if (result == ExportResult::Success && !videoExportEnabled) {
                    if (actualFilename != targetFilename) {
                        //may fail...
                        ::wxRenameFile(actualFilename.GetFullPath(),
                                       targetFilename.GetFullPath(),
                                       true);
                    }
                } else if (result != ExportResult::Success) {
                    ::wxRemoveFile(actualFilename.GetFullPath());
                }
            });

            result = processor->Process(delegate);
            return result;
        });

        auto f = exportTask.get_future();
        ExportResult result = ExportResult::Error;

        if (progress) {
            ProgressDelegate delegate(progress);
            std::thread(std::move(exportTask), std::ref(delegate)).detach();
            while (f.wait_for(std::chrono::milliseconds(50)) != std::future_status::ready) {}
            result = f.get();
        } else {
            DialogExportProgressDelegate delegate;
            std::thread(std::move(exportTask), std::ref(delegate)).detach();
            while (f.wait_for(std::chrono::milliseconds(50)) != std::future_status::ready) {
                delegate.UpdateUI();
            }
            result = f.get();
        }

        if (result != ExportResult::Success) {
            return muse::make_ret(muse::Ret::Code::InternalError);
        }

        if (videoExport.enabled) {
            const std::string remuxError = videoExport.encodingOptions.encodeVideo()
                                           ? encodeLinkedVideo(videoExport.link, videoExport.sourceVideoFilename, videoExport.audioFilename,
                                                               videoExport.targetFilename, videoExport.encodingOptions)
                                           : remuxLinkedVideo(videoExport.link, videoExport.sourceVideoFilename, videoExport.audioFilename,
                                                             videoExport.targetFilename);
            ::wxRemoveFile(videoExport.audioFilename.GetFullPath());
            if (!remuxError.empty()) {
                ::wxRemoveFile(videoExport.targetFilename.GetFullPath());
                return muse::make_ret(muse::Ret::Code::InternalError, remuxError);
            }
        }
    } catch (const ExportException& e) {
        return muse::make_ret(muse::Ret::Code::InternalError, e.What().ToStdString());
    }

    return muse::make_ret(muse::Ret::Code::Ok);
}

std::vector<std::string> Au3Exporter::formatsList() const
{
    std::vector<std::string> formatsList;
    for (auto [plugin, formatIndex] : ExportPluginRegistry::Get()) {
        formatsList.push_back(formatDescription(*plugin, formatIndex));
    }

    return formatsList;
}

int Au3Exporter::formatIndex(const std::string& format) const
{
    return findFormatByName(format).formatIndex;
}

std::vector<std::string> Au3Exporter::formatExtensions(const std::string& format) const
{
    for (auto [plugin, formatIndex] : ExportPluginRegistry::Get()) {
        if (formatDescription(*plugin, formatIndex) == format) {
            auto extensions = plugin->GetFormatInfo(formatIndex).extensions;
            if (!extensions.empty()) {
                std::vector<std::string> result;
                for (const auto& ext : extensions) {
                    result.push_back(ext.ToStdString());
                }
                return result;
            }
        }
    }

    return {};
}

std::vector<std::string> Au3Exporter::cloudPreferredAudioFormats() const
{
    const auto& registry = ExportPluginRegistry::Get();

    std::vector<std::string> result;
    for (const auto& mimeType : cloudConfiguration()->preferredAudioFormats()) {
        for (auto [plugin, formatIndex] : registry) {
            for (const auto& mime : plugin->GetMimeTypes(formatIndex)) {
                if (mime == mimeType) {
                    result.push_back(plugin->GetFormatInfo(formatIndex).description.msgid().toStdString());
                    break;
                }
            }
        }
    }

    if (result.empty()) {
        // Fallback to all formats if no preferred formats are found
        for (auto [plugin, formatIndex] : registry) {
            result.push_back(plugin->GetFormatInfo(formatIndex).description.msgid().toStdString());
        }
    }

    return result;
}

ExportParameters Au3Exporter::cloudExportParameters(const std::string& format) const
{
    const ExportPlugin* plugin = nullptr;
    int fmt = -1;

    for (auto [p, formatIndex] : ExportPluginRegistry::Get()) {
        if (p->GetFormatInfo(formatIndex).description.msgid().toStdString() == format) {
            plugin = p;
            fmt = formatIndex;
            break;
        }
    }

    if (!plugin) {
        return {};
    }

    for (const auto& mimeType : plugin->GetMimeTypes(fmt)) {
        auto config = cloudConfiguration()->exportConfig(mimeType);
        ExportProcessor::Parameters au3Params;
        if (plugin->ParseConfig(fmt, config, au3Params)) {
            ExportParameters result;
            for (const auto& [id, val] : au3Params) {
                result.emplace_back(id, std::visit([](auto v) -> OptionValue { return v; }, val));
            }
            return result;
        }
    }

    return {};
}

bool Au3Exporter::isCustomFFmpegExportFormat() const
{
    OptionsEditorUPtr editor = optionsEditor();
    if (!editor) {
        return false;
    }

    return editor->GetName() == "custom_ffmpeg";
}

bool Au3Exporter::isOggExportFormat() const
{
    OptionsEditorUPtr editor = optionsEditor();
    if (!editor) {
        return false;
    }

    return editor->GetName() == "ogg";
}

bool Au3Exporter::hasMetadata() const
{
    std::string format = exportConfiguration()->currentFormat();

    for (auto [plugin, formatIndex] : ExportPluginRegistry::Get()) {
        if (formatDescription(*plugin, formatIndex) == format) {
            return plugin->GetFormatInfo(formatIndex).canMetaData;
        }
    }

    return false;
}

int Au3Exporter::maxChannels() const
{
    std::string format = exportConfiguration()->currentFormat();

    for (auto [plugin, formatIndex] : ExportPluginRegistry::Get()) {
        if (formatDescription(*plugin, formatIndex) == format) {
            return plugin->GetFormatInfo(formatIndex).maxChannels;
        }
    }

    return 1;
}

std::vector<int> Au3Exporter::sampleRateList() const
{
    OptionsEditorUPtr editor = optionsEditor();
    if (!editor) {
        return {};
    }

    return editor->GetSampleRateList();
}

int Au3Exporter::optionsCount() const
{
    OptionsEditorUPtr editor = optionsEditor();
    if (!editor) {
        return 0;
    }

    return editor->GetOptionsCount();
}

std::optional<au::importexport::ExportOption> Au3Exporter::option(int i) const
{
    OptionsEditorUPtr editor = optionsEditor();
    if (!editor) {
        return std::nullopt;
    }

    ::ExportOption opt;

    if (editor->GetOption(i, opt)) {
        std::string title = opt.title.translated().toStdString();
        std::vector<std::string> names;
        for (const auto& name : opt.names) {
            names.push_back(name.translated().toStdString());
        }

        return ExportOption { opt.id,
                              title,
                              opt.flags,
                              opt.values,
                              names };
    }

    return std::nullopt;
}

std::optional<au::importexport::OptionValue> Au3Exporter::value(int id) const
{
    OptionsEditorUPtr editor = optionsEditor();
    if (!editor) {
        return std::nullopt;
    }

    ::ExportValue val;

    if (editor->GetValue(id, val)) {
        return val;
    }

    return std::nullopt;
}

void Au3Exporter::setValue(int id, const OptionValue& value)
{
    OptionsEditorUPtr editor = optionsEditor();
    if (!editor) {
        return;
    }

    ::ExportValue val = value;

    editor->SetValue(id, val);
    editor->Store(*gPrefs);
}

OptionsEditorUPtr Au3Exporter::optionsEditor() const
{
    std::string format = exportConfiguration()->currentFormat();
    ExportFormatRef ref = findFormatByName(format);

    if (!ref.isValid()) {
        return nullptr;
    }

    auto editor = ref.plugin->CreateOptionsEditor(ref.formatIndex, nullptr);
    if (!editor) {
        LOGE() << "error: failed to create options editor";
        return nullptr;
    }

    editor->Load(*gPrefs);

    return editor;
}

std::vector<bool> Au3Exporter::prepareChannelMask() const
{
    Au3Project* project = reinterpret_cast<Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
    IF_ASSERT_FAILED(project) {
        return {};
    }

    auto tracks = TrackList::Get(*project).Any<WaveTrack>();
    std::vector<bool> channelMask(
        tracks.sum([](const auto track) { return track->NChannels(); }),
        false);
    unsigned trackIndex = 0;
    for (const auto track : tracks) {
        if (track->GetSolo()) {
            channelMask.assign(channelMask.size(), false);
            for (unsigned i = 0; i < track->NChannels(); ++i) {
                channelMask[trackIndex++] = true;
            }
            break;
        }
        if (!track->GetMute() && (!m_selectedOnly || track->GetSelected())) {
            for (unsigned i = 0; i < track->NChannels(); ++i) {
                channelMask[trackIndex++] = true;
            }
        } else {
            trackIndex += track->NChannels();
        }
    }

    return channelMask;
}
