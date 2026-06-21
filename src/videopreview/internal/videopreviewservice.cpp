/*
* Audacity: A Digital Audio Editor
*/
#include "videopreviewservice.h"

#include <algorithm>
#include <cmath>
#include <thread>
#include <vector>

#include <QFileInfo>
#include <QMetaObject>

#include "au3-project/Project.h"
#include "project/iaudacityproject.h"

#include "au3/videolinkstorage.h"
#include "ffmpegvideodecoder.h"

using namespace au::videopreview;

namespace {
constexpr double EPS = 1e-7;

double duration(double start, double end)
{
    return std::max(0.0, end - start);
}

bool sameDuration(double a, double b)
{
    return std::abs(a - b) < EPS;
}

double overlapAmount(double aStart, double aEnd, double bStart, double bEnd)
{
    return std::max(0.0, std::min(aEnd, bEnd) - std::max(aStart, bStart));
}
}

VideoPreviewService::VideoPreviewService(const muse::modularity::ContextPtr& ctx)
    : QObject(nullptr), muse::Contextable(ctx)
{
}

VideoPreviewService::~VideoPreviewService()
{
    detachTrackeditProject();
    if (m_storage) {
        m_storage->linkChanged().disconnect(this);
    }
}

void VideoPreviewService::init()
{
    globalContext()->currentProjectChanged().onNotify(this, [this]() {
        onCurrentProjectChanged();
    });

    if (globalContext()->playbackState()) {
        globalContext()->playbackState()->playbackPositionChanged().onReceive(this, [this](muse::secs_t seconds) {
            setProjectTime(seconds.to_double());
        });
    }

    onCurrentProjectChanged();
}

void VideoPreviewService::onCurrentProjectChanged()
{
    detachTrackeditProject();

    if (m_storage) {
        m_storage->linkChanged().disconnect(this);
    }

    m_au3Project = nullptr;
    m_storage = nullptr;
    m_link = {};
    m_recentSegments.clear();
    m_decodeGeneration++;
    m_decodeInFlight = false;
    m_pendingProjectTime.reset();
    clearFrame();
    setState(VideoPreviewState::Empty);

    const au::project::IAudacityProjectPtr project = globalContext()->currentProject();
    if (!project) {
        m_linkChanged.notify();
        return;
    }

    m_au3Project = reinterpret_cast<AudacityProject*>(project->au3ProjectPtr());
    if (!m_au3Project) {
        m_linkChanged.notify();
        return;
    }

    m_storage = &VideoLinkStorage::Get(*m_au3Project);
    m_storage->linkChanged().onNotify(this, [this]() {
        if (!m_ignoreStorageNotification) {
            onStorageLinkChanged(false);
        }
    });

    onStorageLinkChanged(true);
    attachTrackeditProject();
}

void VideoPreviewService::detachTrackeditProject()
{
    if (!m_trackeditProject) {
        return;
    }

    m_trackeditProject->trackAdded().disconnect(this);
    m_trackeditProject->trackRemoved().disconnect(this);
    m_trackeditProject->trackClipListChanged().disconnect(this);

    for (trackedit::TrackId trackId : m_subscribedTrackIds) {
        m_trackeditProject->clipList(trackId).disconnect(this);
    }

    m_subscribedTrackIds.clear();
    m_trackeditProject.reset();
}

void VideoPreviewService::attachTrackeditProject()
{
    m_trackeditProject = globalContext()->currentTrackeditProject();
    if (!m_trackeditProject) {
        return;
    }

    for (trackedit::TrackId trackId : m_trackeditProject->trackIdList()) {
        subscribeToTrack(trackId);
    }

    m_trackeditProject->trackAdded().onReceive(this, [this](const trackedit::Track& track) {
        subscribeToTrack(track.id);
        updateSegmentMapFromProject(true);
    });

    m_trackeditProject->trackRemoved().onReceive(this, [this](const trackedit::Track&) {
        updateSegmentMapFromProject(true);
    });

    m_trackeditProject->trackClipListChanged().onReceive(this, [this](const trackedit::Track&) {
        updateSegmentMapFromProject(true);
    });
}

void VideoPreviewService::subscribeToTrack(au::trackedit::TrackId trackId)
{
    if (!m_trackeditProject || m_subscribedTrackIds.count(trackId) != 0) {
        return;
    }

    auto clips = m_trackeditProject->clipList(trackId);
    clips.onItemChanged(this, [this](const au::trackedit::Clip&) {
        updateSegmentMapFromProject(true);
    });
    clips.onItemAdded(this, [this](const au::trackedit::Clip&) {
        updateSegmentMapFromProject(true);
    });
    clips.onItemRemoved(this, [this](const au::trackedit::Clip&) {
        updateSegmentMapFromProject(true);
    });

    m_subscribedTrackIds.insert(trackId);
}

void VideoPreviewService::onStorageLinkChanged(bool probe)
{
    if (!m_storage) {
        return;
    }

    m_link = m_storage->link();
    m_recentSegments = m_link.segments;
    m_linkChanged.notify();

    if (!m_link.isValid()) {
        clearFrame();
        setState(VideoPreviewState::Empty);
        return;
    }

    if (!QFileInfo::exists(m_link.sourcePath.toQString())) {
        clearFrame();
        setState(VideoPreviewState::MissingFile);
        return;
    }

    if (probe) {
        const VideoProbeResult probeResult = FFmpegVideoDecoder::probe(m_link.sourcePath, m_link.streamIndex, m_link.streamId);
        if (!probeResult.isUsable()) {
            clearFrame();
            setState(probeResult.state == VideoPreviewState::Empty ? VideoPreviewState::UnsupportedCodec : probeResult.state);
            return;
        }
    }

    setState(VideoPreviewState::Ready);
    setProjectTime(m_lastProjectTime);
}

void VideoPreviewService::linkImportedVideo(const muse::io::path_t& sourcePath,
                                            const std::vector<au::trackedit::Clip>& clips,
                                            double sourceOriginProjectTime)
{
    if (!m_storage || clips.empty()) {
        return;
    }

    const VideoProbeResult probeResult = FFmpegVideoDecoder::probe(sourcePath);
    if (probeResult.state == VideoPreviewState::Empty) {
        return;
    }

    if (!probeResult.isUsable()) {
        clearFrame();
        setState(probeResult.state);
        return;
    }

    VideoLink link;
    link.sourcePath = sourcePath;
    link.streamIndex = probeResult.streamIndex;
    link.streamId = probeResult.streamId;

    link.segments.reserve(clips.size());
    for (const au::trackedit::Clip& clip : clips) {
        const double clipDuration = duration(clip.startTime, clip.endTime);
        if (!clip.isValid() || clipDuration <= EPS) {
            continue;
        }

        VideoSegment segment;
        segment.clipKey = clip.key;
        segment.title = clip.title;
        segment.projectStart = clip.startTime;
        segment.projectEnd = clip.endTime;
        segment.sourceStart = std::max(0.0, clip.startTime - sourceOriginProjectTime);
        segment.sourceEnd = segment.sourceStart + clipDuration;
        link.segments.push_back(std::move(segment));
    }

    if (!link.isValid()) {
        return;
    }

    commitLink(std::move(link), false);
}

void VideoPreviewService::commitLink(VideoLink link, bool refreshUndoState)
{
    if (!m_storage) {
        return;
    }

    m_recentSegments = m_link.segments;
    m_link = std::move(link);

    m_ignoreStorageNotification = true;
    m_storage->setLink(m_link);
    m_ignoreStorageNotification = false;

    if (refreshUndoState && m_au3Project) {
        modifyVideoLinkUndoState(*m_au3Project);
    }

    m_linkChanged.notify();

    if (!m_link.isValid()) {
        clearFrame();
        setState(VideoPreviewState::Empty);
        return;
    }

    if (!QFileInfo::exists(m_link.sourcePath.toQString())) {
        clearFrame();
        setState(VideoPreviewState::MissingFile);
        return;
    }

    setState(VideoPreviewState::Ready);
    setProjectTime(m_lastProjectTime);
}

std::vector<au::trackedit::Clip> VideoPreviewService::currentClips() const
{
    std::vector<au::trackedit::Clip> result;
    if (!m_trackeditProject) {
        return result;
    }

    for (au::trackedit::TrackId trackId : m_trackeditProject->trackIdList()) {
        const auto clips = m_trackeditProject->clipList(trackId);
        result.insert(result.end(), clips.begin(), clips.end());
    }

    return result;
}

std::optional<VideoSegment> VideoPreviewService::segmentForClip(const au::trackedit::Clip& clip,
                                                                const std::vector<VideoSegment>& sourceSegments) const
{
    const double clipDuration = duration(clip.startTime, clip.endTime);
    if (!clip.isValid() || clipDuration <= EPS) {
        return std::nullopt;
    }

    auto makeFromExisting = [&clip, clipDuration](const VideoSegment& existing) {
        VideoSegment segment = existing;
        const double oldProjectDuration = duration(existing.projectStart, existing.projectEnd);
        const double oldSourceDuration = duration(existing.sourceStart, existing.sourceEnd);
        const double leftDelta = clip.startTime - existing.projectStart;
        const bool movedOnly = sameDuration(clipDuration, oldProjectDuration);

        segment.clipKey = clip.key;
        segment.title = clip.title;
        segment.projectStart = clip.startTime;
        segment.projectEnd = clip.endTime;

        if (movedOnly) {
            segment.sourceStart = existing.sourceStart;
            segment.sourceEnd = existing.sourceEnd;
        } else {
            const double sourceScale = oldProjectDuration > EPS ? oldSourceDuration / oldProjectDuration : 1.0;
            segment.sourceStart = existing.sourceStart + (leftDelta * sourceScale);
            segment.sourceStart = std::max(existing.sourceStart, segment.sourceStart);
            segment.sourceEnd = segment.sourceStart + clipDuration * sourceScale;
            segment.sourceEnd = std::min(existing.sourceEnd, segment.sourceEnd);
        }

        if (segment.sourceEnd <= segment.sourceStart + EPS) {
            segment.sourceEnd = segment.sourceStart + clipDuration;
        }

        return segment;
    };

    for (const VideoSegment& segment : sourceSegments) {
        if (segment.clipKey == clip.key) {
            return makeFromExisting(segment);
        }
    }

    const VideoSegment* best = nullptr;
    double bestOverlap = 0.0;

    for (const VideoSegment& segment : sourceSegments) {
        const double overlap = overlapAmount(clip.startTime, clip.endTime, segment.projectStart, segment.projectEnd);
        const bool sameTrack = segment.clipKey.trackId == clip.key.trackId;
        const bool sameTitle = segment.title == clip.title;
        if (!sameTrack && !sameTitle) {
            continue;
        }

        const double weightedOverlap = overlap * (sameTrack ? 2.0 : 1.0) * (sameTitle ? 1.5 : 1.0);

        if (weightedOverlap > bestOverlap) {
            bestOverlap = weightedOverlap;
            best = &segment;
        }
    }

    if (best && bestOverlap > EPS) {
        return makeFromExisting(*best);
    }

    for (const VideoSegment& segment : sourceSegments) {
        if (segment.title == clip.title && sameDuration(duration(segment.projectStart, segment.projectEnd), clipDuration)) {
            VideoSegment duplicate = segment;
            duplicate.clipKey = clip.key;
            duplicate.title = clip.title;
            duplicate.projectStart = clip.startTime;
            duplicate.projectEnd = clip.endTime;
            return duplicate;
        }
    }

    return std::nullopt;
}

void VideoPreviewService::updateSegmentMapFromProject(bool refreshUndoState)
{
    if (!m_link.isValid() || !m_trackeditProject) {
        return;
    }

    std::vector<VideoSegment> sourceSegments = m_link.segments;
    sourceSegments.insert(sourceSegments.end(), m_recentSegments.begin(), m_recentSegments.end());

    VideoLink nextLink = m_link;
    nextLink.segments.clear();

    const std::vector<au::trackedit::Clip> clips = currentClips();
    nextLink.segments.reserve(clips.size());

    for (const au::trackedit::Clip& clip : clips) {
        if (std::optional<VideoSegment> segment = segmentForClip(clip, sourceSegments)) {
            nextLink.segments.push_back(std::move(*segment));
        }
    }

    commitLink(std::move(nextLink), refreshUndoState);
}

std::optional<double> VideoPreviewService::sourceTimeForProjectTime(double projectSeconds) const
{
    for (const VideoSegment& segment : m_link.segments) {
        if (projectSeconds + EPS < segment.projectStart || projectSeconds > segment.projectEnd + EPS) {
            continue;
        }

        const double projectDuration = duration(segment.projectStart, segment.projectEnd);
        const double sourceDuration = duration(segment.sourceStart, segment.sourceEnd);
        if (projectDuration <= EPS || sourceDuration <= EPS) {
            continue;
        }

        const double normalized = std::clamp((projectSeconds - segment.projectStart) / projectDuration, 0.0, 1.0);
        return segment.sourceStart + normalized * sourceDuration;
    }

    return std::nullopt;
}

void VideoPreviewService::setProjectTime(double seconds)
{
    m_lastProjectTime = seconds;

    if (m_state != VideoPreviewState::Ready || !m_link.isValid()) {
        return;
    }

    const std::optional<double> sourceSeconds = sourceTimeForProjectTime(seconds);
    if (!sourceSeconds.has_value()) {
        clearFrame();
        return;
    }

    if (m_decodeInFlight) {
        m_pendingProjectTime = seconds;
        return;
    }

    requestDecode(*sourceSeconds);
}

void VideoPreviewService::requestDecode(double sourceSeconds)
{
    if (!m_link.isValid()) {
        return;
    }

    m_decodeInFlight = true;
    const uint64_t generation = ++m_decodeGeneration;
    const muse::io::path_t path = m_link.sourcePath;
    const int streamIndex = m_link.streamIndex;
    const int streamId = m_link.streamId;
    std::weak_ptr<VideoPreviewService> weakSelf = weak_from_this();

    std::thread([weakSelf, generation, path, streamIndex, streamId, sourceSeconds]() {
        VideoDecodeResult result = FFmpegVideoDecoder::decodeFrame(path, streamIndex, streamId, sourceSeconds);
        if (auto self = weakSelf.lock()) {
            QMetaObject::invokeMethod(self.get(), [self, generation, result = std::move(result)]() mutable {
                self->onDecodeFinished(generation, std::move(result));
            }, Qt::QueuedConnection);
        }
    }).detach();
}

void VideoPreviewService::onDecodeFinished(uint64_t generation, VideoDecodeResult result)
{
    if (generation != m_decodeGeneration) {
        return;
    }

    m_decodeInFlight = false;

    if (m_pendingProjectTime.has_value()) {
        const double pending = *m_pendingProjectTime;
        m_pendingProjectTime.reset();
        setProjectTime(pending);
        return;
    }

    if (!result.isUsable()) {
        clearFrame();
        setState(result.state);
        return;
    }

    {
        std::scoped_lock lock(m_frameMutex);
        m_currentFrame = std::move(result.frame);
    }

    setState(VideoPreviewState::Ready);
    m_frameChanged.notify();
}

void VideoPreviewService::setState(VideoPreviewState state)
{
    if (m_state == state) {
        return;
    }

    m_state = state;
    m_stateChanged.notify();
}

void VideoPreviewService::clearFrame()
{
    bool hadFrame = false;
    {
        std::scoped_lock lock(m_frameMutex);
        hadFrame = !m_currentFrame.isNull();
        m_currentFrame = {};
    }

    if (hadFrame) {
        m_frameChanged.notify();
    }
}

VideoPreviewState VideoPreviewService::state() const
{
    return m_state;
}

muse::String VideoPreviewService::stateText() const
{
    switch (m_state) {
    case VideoPreviewState::Empty:
        return muse::String::fromAscii("No video linked");
    case VideoPreviewState::Ready:
        return muse::String::fromAscii("No video frame");
    case VideoPreviewState::MissingFile:
        return muse::String::fromAscii("Linked video missing");
    case VideoPreviewState::NoFfmpeg:
        return muse::String::fromAscii("FFmpeg 5+ video support unavailable");
    case VideoPreviewState::UnsupportedCodec:
        return muse::String::fromAscii("Unsupported video");
    case VideoPreviewState::DecodingError:
        return muse::String::fromAscii("Video decode failed");
    }

    return {};
}

muse::io::path_t VideoPreviewService::sourcePath() const
{
    return m_link.sourcePath;
}

double VideoPreviewService::aspectRatio() const
{
    std::scoped_lock lock(m_frameMutex);
    if (!m_currentFrame.isNull() && m_currentFrame.height() > 0) {
        return static_cast<double>(m_currentFrame.width()) / static_cast<double>(m_currentFrame.height());
    }

    return 16.0 / 9.0;
}

QImage VideoPreviewService::currentFrame() const
{
    std::scoped_lock lock(m_frameMutex);
    return m_currentFrame;
}

muse::async::Notification VideoPreviewService::frameChanged() const
{
    return m_frameChanged;
}

muse::async::Notification VideoPreviewService::stateChanged() const
{
    return m_stateChanged;
}

muse::async::Notification VideoPreviewService::linkChanged() const
{
    return m_linkChanged;
}
