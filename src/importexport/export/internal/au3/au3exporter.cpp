/*
* Audacity: A Digital Audio Editor
*/

#include "au3exporter.h"

#include <algorithm>
#include <array>
#include <cctype>
#include <cmath>
#include <optional>

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

ExportPlugin* formatPlugin(const std::string& format)
{
    for (auto [plugin, formatIndex] : ExportPluginRegistry::Get()) {
        if (plugin->GetFormatInfo(formatIndex).description.msgid().toStdString() == format) {
            return plugin;
        }
    }

    return nullptr;
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

struct LinkedVideoExport
{
    bool enabled = false;
    VideoLink link;
    wxFileName sourceVideoFilename;
    wxFileName audioFilename;
    wxFileName targetFilename;
};

LinkedVideoExport linkedVideoExport(const au::videopreview::IVideoPreviewService* videoPreviewService,
                                    const ExportPlugin& plugin, int format, ExportProcessType processType,
                                    const wxFileName& targetFilename)
{
    LinkedVideoExport result;
    if (!videoPreviewService || processType != ExportProcessType::FULL_PROJECT_AUDIO
        || !isVideoContainerExtension(targetFilename.GetExt())) {
        return result;
    }

    const VideoLink link = videoPreviewService->link();
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
    const std::string formatName = options.count(OptionKey::Format)
                                   ? options.at(OptionKey::Format).toString()
                                   : exportConfiguration()->currentFormat();

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
        const int fmt = formatIndex(formatName);
        const ExportPlugin* plugin = formatPlugin(formatName);
        if (plugin) {
            auto editor = plugin->CreateOptionsEditor(fmt, nullptr);
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

    int formatIdx = formatIndex(formatName);
    if (formatIdx == -1) {
        return muse::make_ret(muse::Ret::Code::InternalError);
    }
    m_format = formatIdx;

    m_plugin = formatPlugin(formatName);
    if (!m_plugin) {
        return muse::make_ret(muse::Ret::Code::InternalError);
    }

    const LinkedVideoExport videoExport = linkedVideoExport(videoPreviewService().get(), *m_plugin, formatIdx, processType, wxfilename);
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
            const std::string remuxError = remuxLinkedVideo(videoExport.link, videoExport.sourceVideoFilename, videoExport.audioFilename,
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
        formatsList.push_back(plugin->GetFormatInfo(formatIndex).description.msgid().toStdString());
    }

    return formatsList;
}

int Au3Exporter::formatIndex(const std::string& format) const
{
    for (auto [plugin, formatIndex] : ExportPluginRegistry::Get()) {
        if (plugin->GetFormatInfo(formatIndex).description.msgid().toStdString() == format) {
            return formatIndex;
        }
    }

    return -1;
}

std::vector<std::string> Au3Exporter::formatExtensions(const std::string& format) const
{
    for (auto [plugin, formatIndex] : ExportPluginRegistry::Get()) {
        if (plugin->GetFormatInfo(formatIndex).description.msgid().toStdString() == format) {
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
        if (plugin->GetFormatInfo(formatIndex).description.msgid().toStdString() == format) {
            return plugin->GetFormatInfo(formatIndex).canMetaData;
        }
    }

    return false;
}

int Au3Exporter::maxChannels() const
{
    std::string format = exportConfiguration()->currentFormat();

    for (auto [plugin, formatIndex] : ExportPluginRegistry::Get()) {
        if (plugin->GetFormatInfo(formatIndex).description.msgid().toStdString() == format) {
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

    for (auto [plugin, formatIndex] : ExportPluginRegistry::Get()) {
        if (plugin->GetFormatInfo(formatIndex).description.msgid().toStdString() == format) {
            auto editor = plugin->CreateOptionsEditor(formatIndex, nullptr);
            if (!editor) {
                LOGE() << "error: failed to create options editor";
                return nullptr;
            }

            editor->Load(*gPrefs);

            return editor;
        }
    }

    return nullptr;
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
