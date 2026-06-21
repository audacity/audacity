/*
* Audacity: A Digital Audio Editor
*/
#include "videopreviewservice.h"

#include <algorithm>
#include <cmath>
#include <iterator>
#include <limits>
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
constexpr au::trackedit::TrackId VIDEO_TRACK_ID = -1000000;
constexpr au::trackedit::TrackItemId FIRST_VIDEO_SEGMENT_ID = 1;
constexpr au::trackedit::ClipColorIndex VIDEO_CLIP_COLOR = 4;

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

double sourceAtProjectTime(const VideoSegment& segment, double projectTime)
{
    const double projectDuration = duration(segment.projectStart, segment.projectEnd);
    const double sourceDuration = duration(segment.sourceStart, segment.sourceEnd);
    if (projectDuration <= EPS || sourceDuration <= EPS) {
        return segment.sourceStart;
    }

    const double normalized = std::clamp((projectTime - segment.projectStart) / projectDuration, 0.0, 1.0);
    return segment.sourceStart + normalized * sourceDuration;
}

au::trackedit::TrackItemId nextSegmentId(const std::vector<VideoSegment>& segments)
{
    au::trackedit::TrackItemId next = FIRST_VIDEO_SEGMENT_ID;
    for (const VideoSegment& segment : segments) {
        next = std::max(next, segment.clipKey.itemId + 1);
    }
    return next;
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

    attachTrackeditProject();
    onStorageLinkChanged(true);
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

    const VideoLink oldLink = m_link;
    m_link = normalizedLink(m_storage->link());
    m_recentSegments = m_link.segments;

    m_ignoreStorageNotification = true;
    m_storage->setLink(m_link);
    m_ignoreStorageNotification = false;

    notifyTrackeditAboutLinkChange(oldLink, m_link);
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
    link.trackTitle = muse::String::fromAscii("Video");
    link.streamIndex = probeResult.streamIndex;
    link.streamId = probeResult.streamId;

    double projectStart = std::numeric_limits<double>::max();
    double projectEnd = 0.0;
    muse::String title = muse::String::fromQString(QFileInfo(sourcePath.toQString()).fileName());

    for (const au::trackedit::Clip& clip : clips) {
        const double clipDuration = duration(clip.startTime, clip.endTime);
        if (!clip.isValid() || clipDuration <= EPS) {
            continue;
        }

        projectStart = std::min(projectStart, clip.startTime);
        projectEnd = std::max(projectEnd, clip.endTime);
        if (!clip.title.empty()) {
            title = clip.title;
        }
    }

    const double clipDuration = duration(projectStart, projectEnd);
    if (clipDuration <= EPS) {
        return;
    }

    const int64_t groupId = m_trackeditProject ? m_trackeditProject->createNewGroupID() : -1;
    if (clipsInteraction() && groupId != -1) {
        for (const au::trackedit::Clip& clip : clips) {
            if (clip.isValid()) {
                clipsInteraction()->setClipGroupId(clip.key, groupId);
            }
        }
    }

    VideoSegment segment;
    segment.clipKey = au::trackedit::ClipKey(VIDEO_TRACK_ID, FIRST_VIDEO_SEGMENT_ID);
    segment.title = title.empty() ? muse::String::fromAscii("Video") : title;
    segment.groupId = groupId;
    segment.colorIndex = VIDEO_CLIP_COLOR;
    segment.projectStart = projectStart;
    segment.projectEnd = projectEnd;
    segment.sourceStart = std::max(0.0, projectStart - sourceOriginProjectTime);
    segment.sourceEnd = segment.sourceStart + clipDuration;
    link.segments.push_back(std::move(segment));

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

    link = normalizedLink(std::move(link));
    const VideoLink oldLink = m_link;
    m_recentSegments = m_link.segments;
    m_link = std::move(link);

    m_ignoreStorageNotification = true;
    m_storage->setLink(m_link);
    m_ignoreStorageNotification = false;

    if (refreshUndoState && m_au3Project) {
        modifyVideoLinkUndoState(*m_au3Project);
    }

    notifyTrackeditAboutLinkChange(oldLink, m_link);
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

au::trackedit::Track VideoPreviewService::videoTrack(const VideoLink& link) const
{
    au::trackedit::Track track;
    track.id = VIDEO_TRACK_ID;
    track.title = link.trackTitle.empty() ? muse::String::fromAscii("Video") : link.trackTitle;
    track.type = au::trackedit::TrackType::Video;
    track.colorIndex = VIDEO_CLIP_COLOR;
    return track;
}

au::trackedit::Clips VideoPreviewService::videoClips(const VideoLink& link) const
{
    au::trackedit::Clips clips;
    if (!link.isValid()) {
        return clips;
    }

    clips.reserve(link.segments.size());
    for (const VideoSegment& segment : link.segments) {
        if (!segment.isValid()) {
            continue;
        }

        au::trackedit::Clip clip;
        clip.key = segment.clipKey;
        clip.title = segment.title.empty() ? muse::String::fromAscii("Video") : segment.title;
        clip.colorIndex = segment.colorIndex;
        clip.groupId = segment.groupId;
        clip.startTime = segment.projectStart;
        clip.endTime = segment.projectEnd;
        clip.stereo = false;
        const double sourceDuration = duration(segment.sourceStart, segment.sourceEnd);
        const double projectDuration = duration(segment.projectStart, segment.projectEnd);
        clip.speed = sourceDuration > EPS ? projectDuration / sourceDuration : 1.0;
        clips.push_back(std::move(clip));
    }

    return clips;
}

VideoLink VideoPreviewService::normalizedLink(VideoLink link) const
{
    if (link.trackTitle.empty()) {
        link.trackTitle = muse::String::fromAscii("Video");
    }

    au::trackedit::TrackItemId nextId = FIRST_VIDEO_SEGMENT_ID;
    std::set<au::trackedit::TrackItemId> usedIds;
    for (VideoSegment& segment : link.segments) {
        segment.clipKey.trackId = VIDEO_TRACK_ID;
        if (segment.clipKey.itemId == au::trackedit::INVALID_TRACK_ITEM || usedIds.count(segment.clipKey.itemId) != 0) {
            while (usedIds.count(nextId) != 0) {
                ++nextId;
            }
            segment.clipKey.itemId = nextId;
        }
        usedIds.insert(segment.clipKey.itemId);
        nextId = std::max(nextId, segment.clipKey.itemId + 1);
        if (segment.colorIndex == au::trackedit::CLIP_COLOR_INDEX_NONE) {
            segment.colorIndex = VIDEO_CLIP_COLOR;
        }
        if (segment.title.empty()) {
            segment.title = muse::String::fromAscii("Video");
        }
    }

    return link;
}

void VideoPreviewService::notifyTrackeditAboutLinkChange(const VideoLink& oldLink, const VideoLink& newLink)
{
    const au::trackedit::ITrackeditProjectPtr project = m_trackeditProject ? m_trackeditProject : globalContext()->currentTrackeditProject();
    if (!project) {
        return;
    }

    const bool hadTrack = oldLink.isValid();
    const bool hasTrackNow = newLink.isValid();
    const au::trackedit::Track oldTrack = videoTrack(oldLink);
    const au::trackedit::Track newTrack = videoTrack(newLink);
    const au::trackedit::Clips oldClips = videoClips(oldLink);
    const au::trackedit::Clips newClips = videoClips(newLink);

    if (hadTrack && !hasTrackNow) {
        for (const au::trackedit::Clip& clip : oldClips) {
            project->notifyAboutClipRemoved(clip);
        }
        project->notifyAboutTrackRemoved(oldTrack);
        m_videoClipsChanged.changed();
        return;
    }

    if (!hadTrack && hasTrackNow) {
        project->notifyAboutTrackAdded(newTrack);
        for (const au::trackedit::Clip& clip : newClips) {
            project->notifyAboutClipAdded(clip);
        }
        project->notifyAboutTrackClipListChanged(newTrack);
        m_videoClipsChanged.changed();
        return;
    }

    if (hadTrack && hasTrackNow) {
        project->notifyAboutTrackChanged(newTrack);
        for (const au::trackedit::Clip& oldClip : oldClips) {
            const auto it = std::find_if(newClips.begin(), newClips.end(), [&oldClip](const au::trackedit::Clip& clip) {
                return clip.key == oldClip.key;
            });

            if (it == newClips.end()) {
                project->notifyAboutClipRemoved(oldClip);
            }
        }

        for (const au::trackedit::Clip& newClip : newClips) {
            const auto it = std::find_if(oldClips.begin(), oldClips.end(), [&newClip](const au::trackedit::Clip& clip) {
                return clip.key == newClip.key;
            });

            if (it == oldClips.end()) {
                project->notifyAboutClipAdded(newClip);
            } else {
                project->notifyAboutClipChanged(newClip);
            }
        }

        project->notifyAboutTrackClipListChanged(newTrack);
        m_videoClipsChanged.changed();
    }
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
    Q_UNUSED(refreshUndoState);
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

au::trackedit::TrackIdList VideoPreviewService::trackIdList() const
{
    return m_link.isValid() ? au::trackedit::TrackIdList { VIDEO_TRACK_ID } : au::trackedit::TrackIdList {};
}

au::trackedit::TrackList VideoPreviewService::trackList() const
{
    return m_link.isValid() ? au::trackedit::TrackList { videoTrack(m_link) } : au::trackedit::TrackList {};
}

std::optional<au::trackedit::Track> VideoPreviewService::track(au::trackedit::TrackId trackId) const
{
    if (!hasTrack(trackId)) {
        return std::nullopt;
    }

    return videoTrack(m_link);
}

bool VideoPreviewService::hasTrack(au::trackedit::TrackId trackId) const
{
    return trackId == VIDEO_TRACK_ID && m_link.isValid();
}

au::trackedit::Clip VideoPreviewService::clip(const au::trackedit::ClipKey& key) const
{
    const au::trackedit::Clips clips = videoClips(m_link);
    const auto it = std::find_if(clips.begin(), clips.end(), [&key](const au::trackedit::Clip& clip) {
        return clip.key == key;
    });

    return it == clips.end() ? au::trackedit::Clip {} : *it;
}

muse::async::NotifyList<au::trackedit::Clip> VideoPreviewService::clipList(au::trackedit::TrackId trackId) const
{
    muse::async::NotifyList<au::trackedit::Clip> notifyList;
    const au::trackedit::Clips clips = hasTrack(trackId) ? videoClips(m_link) : au::trackedit::Clips {};

    notifyList.reserve(clips.size());
    for (const au::trackedit::Clip& clip : clips) {
        notifyList.push_back(clip);
    }

    notifyList.setNotify(m_videoClipsChanged.notify());
    return notifyList;
}

bool VideoPreviewService::hasClip(const au::trackedit::ClipKey& key) const
{
    return key.trackId == VIDEO_TRACK_ID
           && std::any_of(m_link.segments.begin(), m_link.segments.end(), [&key](const VideoSegment& segment) {
        return segment.clipKey == key;
    });
}

au::trackedit::ClipKeyList VideoPreviewService::clipsOnTrack(au::trackedit::TrackId trackId) const
{
    au::trackedit::ClipKeyList result;
    if (!hasTrack(trackId)) {
        return result;
    }

    result.reserve(m_link.segments.size());
    for (const VideoSegment& segment : m_link.segments) {
        if (segment.isValid()) {
            result.push_back(segment.clipKey);
        }
    }

    return result;
}

std::vector<int64_t> VideoPreviewService::groupsIdsList() const
{
    std::vector<int64_t> groups;
    for (const VideoSegment& segment : m_link.segments) {
        if (segment.groupId != -1 && std::find(groups.begin(), groups.end(), segment.groupId) == groups.end()) {
            groups.push_back(segment.groupId);
        }
    }

    return groups;
}

int64_t VideoPreviewService::clipGroupId(const au::trackedit::ClipKey& clipKey) const
{
    const auto it = std::find_if(m_link.segments.begin(), m_link.segments.end(), [&clipKey](const VideoSegment& segment) {
        return segment.clipKey == clipKey;
    });

    return it == m_link.segments.end() ? -1 : it->groupId;
}

void VideoPreviewService::setClipGroupId(const au::trackedit::ClipKey& clipKey, int64_t id)
{
    VideoLink link = m_link;
    auto it = std::find_if(link.segments.begin(), link.segments.end(), [&clipKey](const VideoSegment& segment) {
        return segment.clipKey == clipKey;
    });

    if (it == link.segments.end()) {
        return;
    }

    it->groupId = static_cast<int>(id);
    commitLink(std::move(link), true);
}

au::trackedit::ClipKeyList VideoPreviewService::clipsInGroup(int64_t id) const
{
    au::trackedit::ClipKeyList result;
    if (id == -1) {
        return result;
    }

    for (const VideoSegment& segment : m_link.segments) {
        if (segment.groupId == id) {
            result.push_back(segment.clipKey);
        }
    }

    return result;
}

au::trackedit::secs_t VideoPreviewService::totalTime() const
{
    double endTime = 0.0;
    for (const VideoSegment& segment : m_link.segments) {
        endTime = std::max(endTime, segment.projectEnd);
    }

    return endTime;
}

au::trackedit::secs_t VideoPreviewService::clipStartTime(const au::trackedit::ClipKey& clipKey) const
{
    const au::trackedit::Clip clip = this->clip(clipKey);
    return clip.isValid() ? clip.startTime : -1.0;
}

au::trackedit::secs_t VideoPreviewService::clipEndTime(const au::trackedit::ClipKey& clipKey) const
{
    const au::trackedit::Clip clip = this->clip(clipKey);
    return clip.isValid() ? clip.endTime : -1.0;
}

au::trackedit::secs_t VideoPreviewService::clipDuration(const au::trackedit::ClipKey& clipKey) const
{
    const au::trackedit::Clip clip = this->clip(clipKey);
    return clip.isValid() ? duration(clip.startTime, clip.endTime) : -1.0;
}

bool VideoPreviewService::changeClipStartTime(const au::trackedit::ClipKey& clipKey, au::trackedit::secs_t newStartTime, bool completed)
{
    VideoLink link = m_link;
    auto it = std::find_if(link.segments.begin(), link.segments.end(), [&clipKey](const VideoSegment& segment) {
        return segment.clipKey == clipKey;
    });

    if (it == link.segments.end()) {
        return false;
    }

    const double segmentDuration = duration(it->projectStart, it->projectEnd);
    it->projectStart = std::max(0.0, newStartTime.to_double());
    it->projectEnd = it->projectStart + segmentDuration;
    commitLink(std::move(link), completed);
    return true;
}

bool VideoPreviewService::changeClipTitle(const au::trackedit::ClipKey& clipKey, const muse::String& newTitle)
{
    VideoLink link = m_link;
    auto it = std::find_if(link.segments.begin(), link.segments.end(), [&clipKey](const VideoSegment& segment) {
        return segment.clipKey == clipKey;
    });

    if (it == link.segments.end()) {
        return false;
    }

    it->title = newTitle;
    commitLink(std::move(link), true);
    return true;
}

bool VideoPreviewService::changeClipColor(const au::trackedit::ClipKey& clipKey, au::trackedit::ClipColorIndex colorIndex)
{
    VideoLink link = m_link;
    auto it = std::find_if(link.segments.begin(), link.segments.end(), [&clipKey](const VideoSegment& segment) {
        return segment.clipKey == clipKey;
    });

    if (it == link.segments.end()) {
        return false;
    }

    it->colorIndex = colorIndex == au::trackedit::CLIP_COLOR_INDEX_NONE ? VIDEO_CLIP_COLOR : colorIndex;
    commitLink(std::move(link), true);
    return true;
}

std::optional<au::trackedit::TimeSpan> VideoPreviewService::removeClip(const au::trackedit::ClipKey& clipKey)
{
    const au::trackedit::Clip clip = this->clip(clipKey);
    if (!clip.isValid()) {
        return std::nullopt;
    }

    if (!removeClips({ clipKey }, false)) {
        return std::nullopt;
    }

    return au::trackedit::TimeSpan { clip.startTime, clip.endTime };
}

bool VideoPreviewService::removeClips(const au::trackedit::ClipKeyList& clipKeyList, bool moveClips)
{
    if (clipKeyList.empty() || !m_link.isValid()) {
        return false;
    }

    VideoLink link = m_link;
    std::vector<std::pair<double, double> > removedRanges;
    std::vector<VideoSegment> keptSegments;
    keptSegments.reserve(link.segments.size());

    for (const VideoSegment& segment : link.segments) {
        const bool remove = std::find(clipKeyList.begin(), clipKeyList.end(), segment.clipKey) != clipKeyList.end();
        if (remove) {
            removedRanges.emplace_back(segment.projectStart, segment.projectEnd);
        } else {
            keptSegments.push_back(segment);
        }
    }

    if (removedRanges.empty()) {
        return false;
    }

    if (moveClips) {
        std::sort(removedRanges.begin(), removedRanges.end());
        for (const auto& [start, end] : removedRanges) {
            const double removedDuration = duration(start, end);
            for (VideoSegment& segment : keptSegments) {
                if (segment.projectStart >= end - EPS) {
                    segment.projectStart = std::max(0.0, segment.projectStart - removedDuration);
                    segment.projectEnd = std::max(segment.projectStart, segment.projectEnd - removedDuration);
                }
            }
        }
    }

    link.segments = std::move(keptSegments);
    commitLink(std::move(link), true);
    return true;
}

muse::RetVal<au::trackedit::ClipKeyList> VideoPreviewService::moveClips(const au::trackedit::ClipKeyList& clipKeyList,
                                                                        au::trackedit::secs_t timePositionOffset,
                                                                        int trackPositionOffset, bool completed,
                                                                        bool& clipsMovedToOtherTracks)
{
    Q_UNUSED(trackPositionOffset);
    clipsMovedToOtherTracks = false;

    if (clipKeyList.empty() || !m_link.isValid()) {
        return muse::RetVal<au::trackedit::ClipKeyList>::make_ok(clipKeyList);
    }

    double offset = timePositionOffset.to_double();
    double leftmost = std::numeric_limits<double>::max();
    for (const VideoSegment& segment : m_link.segments) {
        if (std::find(clipKeyList.begin(), clipKeyList.end(), segment.clipKey) != clipKeyList.end()) {
            leftmost = std::min(leftmost, segment.projectStart);
        }
    }

    if (leftmost == std::numeric_limits<double>::max()) {
        return muse::RetVal<au::trackedit::ClipKeyList>::make_ok(clipKeyList);
    }

    if (leftmost + offset < 0.0) {
        offset = -leftmost;
    }

    VideoLink link = m_link;
    for (VideoSegment& segment : link.segments) {
        if (std::find(clipKeyList.begin(), clipKeyList.end(), segment.clipKey) == clipKeyList.end()) {
            continue;
        }

        segment.projectStart += offset;
        segment.projectEnd += offset;
    }

    commitLink(std::move(link), completed);
    return muse::RetVal<au::trackedit::ClipKeyList>::make_ok(clipKeyList);
}

bool VideoPreviewService::trimClipsLeft(const au::trackedit::ClipKeyList& clipKeyList, au::trackedit::secs_t deltaSec,
                                        au::trackedit::secs_t minClipDuration, bool completed)
{
    if (clipKeyList.empty() || !m_link.isValid()) {
        return false;
    }

    VideoLink link = m_link;
    bool changed = false;
    const double minimumDuration = std::max(EPS, minClipDuration.to_double());

    for (VideoSegment& segment : link.segments) {
        if (std::find(clipKeyList.begin(), clipKeyList.end(), segment.clipKey) == clipKeyList.end()) {
            continue;
        }

        double delta = deltaSec.to_double();
        const double maxShrink = std::max(0.0, duration(segment.projectStart, segment.projectEnd) - minimumDuration);
        delta = std::min(delta, maxShrink);
        if (segment.projectStart + delta < 0.0) {
            delta = -segment.projectStart;
        }
        if (segment.sourceStart + delta < 0.0) {
            delta = -segment.sourceStart;
        }

        if (std::abs(delta) <= EPS) {
            continue;
        }

        segment.projectStart += delta;
        segment.sourceStart += delta;
        changed = true;
    }

    if (changed) {
        commitLink(std::move(link), completed);
    }
    return changed;
}

bool VideoPreviewService::trimClipsRight(const au::trackedit::ClipKeyList& clipKeyList, au::trackedit::secs_t deltaSec,
                                         au::trackedit::secs_t minClipDuration, bool completed)
{
    if (clipKeyList.empty() || !m_link.isValid()) {
        return false;
    }

    VideoLink link = m_link;
    bool changed = false;
    const double minimumDuration = std::max(EPS, minClipDuration.to_double());

    for (VideoSegment& segment : link.segments) {
        if (std::find(clipKeyList.begin(), clipKeyList.end(), segment.clipKey) == clipKeyList.end()) {
            continue;
        }

        double delta = deltaSec.to_double();
        const double maxShrink = std::max(0.0, duration(segment.projectStart, segment.projectEnd) - minimumDuration);
        delta = std::min(delta, maxShrink);

        if (std::abs(delta) <= EPS) {
            continue;
        }

        segment.projectEnd -= delta;
        segment.sourceEnd -= delta;
        if (segment.sourceEnd <= segment.sourceStart + EPS) {
            segment.sourceEnd = segment.sourceStart + minimumDuration;
        }
        changed = true;
    }

    if (changed) {
        commitLink(std::move(link), completed);
    }
    return changed;
}

bool VideoPreviewService::stretchClipsLeft(const au::trackedit::ClipKeyList& clipKeyList, au::trackedit::secs_t deltaSec,
                                           au::trackedit::secs_t minClipDuration, bool completed)
{
    if (clipKeyList.empty() || !m_link.isValid()) {
        return false;
    }

    VideoLink link = m_link;
    bool changed = false;
    const double minimumDuration = std::max(EPS, minClipDuration.to_double());

    for (VideoSegment& segment : link.segments) {
        if (std::find(clipKeyList.begin(), clipKeyList.end(), segment.clipKey) == clipKeyList.end()) {
            continue;
        }

        double delta = deltaSec.to_double();
        const double maxShrink = std::max(0.0, duration(segment.projectStart, segment.projectEnd) - minimumDuration);
        delta = std::min(delta, maxShrink);
        if (segment.projectStart + delta < 0.0) {
            delta = -segment.projectStart;
        }

        if (std::abs(delta) <= EPS) {
            continue;
        }

        segment.projectStart += delta;
        changed = true;
    }

    if (changed) {
        commitLink(std::move(link), completed);
    }
    return changed;
}

bool VideoPreviewService::stretchClipsRight(const au::trackedit::ClipKeyList& clipKeyList, au::trackedit::secs_t deltaSec,
                                            au::trackedit::secs_t minClipDuration, bool completed)
{
    if (clipKeyList.empty() || !m_link.isValid()) {
        return false;
    }

    VideoLink link = m_link;
    bool changed = false;
    const double minimumDuration = std::max(EPS, minClipDuration.to_double());

    for (VideoSegment& segment : link.segments) {
        if (std::find(clipKeyList.begin(), clipKeyList.end(), segment.clipKey) == clipKeyList.end()) {
            continue;
        }

        double delta = deltaSec.to_double();
        const double maxShrink = std::max(0.0, duration(segment.projectStart, segment.projectEnd) - minimumDuration);
        delta = std::min(delta, maxShrink);

        if (std::abs(delta) <= EPS) {
            continue;
        }

        segment.projectEnd -= delta;
        changed = true;
    }

    if (changed) {
        commitLink(std::move(link), completed);
    }
    return changed;
}

bool VideoPreviewService::singleClipOnTrack(au::trackedit::TrackId trackId) const
{
    return hasTrack(trackId) && videoClips(m_link).size() == 1;
}

bool VideoPreviewService::trimTracksData(const au::trackedit::TrackIdList& tracksIds, au::trackedit::secs_t begin,
                                         au::trackedit::secs_t end)
{
    if (std::find(tracksIds.begin(), tracksIds.end(), VIDEO_TRACK_ID) == tracksIds.end() || !m_link.isValid()) {
        return false;
    }

    const double beginTime = begin.to_double();
    const double endTime = end.to_double();
    if (duration(beginTime, endTime) <= EPS) {
        return false;
    }

    VideoLink link = m_link;
    std::vector<VideoSegment> nextSegments;
    nextSegments.reserve(link.segments.size());

    for (const VideoSegment& segment : link.segments) {
        if (segment.projectEnd <= beginTime + EPS || segment.projectStart >= endTime - EPS) {
            continue;
        }

        VideoSegment trimmed = segment;
        if (trimmed.projectStart < beginTime) {
            trimmed.sourceStart = sourceAtProjectTime(segment, beginTime);
            trimmed.projectStart = beginTime;
        }
        if (trimmed.projectEnd > endTime) {
            trimmed.sourceEnd = sourceAtProjectTime(segment, endTime);
            trimmed.projectEnd = endTime;
        }

        if (trimmed.isValid()) {
            nextSegments.push_back(std::move(trimmed));
        }
    }

    link.segments = std::move(nextSegments);
    commitLink(std::move(link), true);
    return true;
}

bool VideoPreviewService::removeTracksData(const au::trackedit::TrackIdList& tracksIds, au::trackedit::secs_t begin,
                                           au::trackedit::secs_t end, bool moveClips)
{
    if (std::find(tracksIds.begin(), tracksIds.end(), VIDEO_TRACK_ID) == tracksIds.end() || !m_link.isValid()) {
        return false;
    }

    const double beginTime = begin.to_double();
    const double endTime = end.to_double();
    const double removedDuration = duration(beginTime, endTime);
    if (removedDuration <= EPS) {
        return false;
    }

    VideoLink link = m_link;
    std::vector<VideoSegment> nextSegments;
    nextSegments.reserve(link.segments.size() + 1);
    au::trackedit::TrackItemId nextId = nextSegmentId(link.segments);

    for (const VideoSegment& segment : link.segments) {
        if (segment.projectEnd <= beginTime + EPS) {
            nextSegments.push_back(segment);
            continue;
        }

        if (segment.projectStart >= endTime - EPS) {
            VideoSegment shifted = segment;
            if (moveClips) {
                shifted.projectStart = std::max(0.0, shifted.projectStart - removedDuration);
                shifted.projectEnd = std::max(shifted.projectStart, shifted.projectEnd - removedDuration);
            }
            nextSegments.push_back(std::move(shifted));
            continue;
        }

        if (segment.projectStart < beginTime - EPS) {
            VideoSegment left = segment;
            left.projectEnd = beginTime;
            left.sourceEnd = sourceAtProjectTime(segment, beginTime);
            nextSegments.push_back(std::move(left));
        }

        if (segment.projectEnd > endTime + EPS) {
            VideoSegment right = segment;
            right.clipKey.itemId = nextId++;
            right.sourceStart = sourceAtProjectTime(segment, endTime);
            right.projectStart = moveClips ? beginTime : endTime;
            right.projectEnd = right.projectStart + duration(endTime, segment.projectEnd);
            nextSegments.push_back(std::move(right));
        }
    }

    link.segments = std::move(nextSegments);
    commitLink(std::move(link), true);
    return true;
}

bool VideoPreviewService::splitTracksAt(const au::trackedit::TrackIdList& tracksIds, std::vector<au::trackedit::secs_t> pivots)
{
    if (std::find(tracksIds.begin(), tracksIds.end(), VIDEO_TRACK_ID) == tracksIds.end() || !m_link.isValid()) {
        return false;
    }

    std::vector<double> splitTimes;
    splitTimes.reserve(pivots.size());
    for (const au::trackedit::secs_t& pivot : pivots) {
        splitTimes.push_back(pivot.to_double());
    }
    std::sort(splitTimes.begin(), splitTimes.end());

    VideoLink link = m_link;
    std::vector<VideoSegment> nextSegments;
    nextSegments.reserve(link.segments.size() + splitTimes.size());
    au::trackedit::TrackItemId nextId = nextSegmentId(link.segments);

    for (const VideoSegment& segment : link.segments) {
        std::vector<double> inside;
        for (double splitTime : splitTimes) {
            if (splitTime > segment.projectStart + EPS && splitTime < segment.projectEnd - EPS) {
                inside.push_back(splitTime);
            }
        }

        if (inside.empty()) {
            nextSegments.push_back(segment);
            continue;
        }

        double partProjectStart = segment.projectStart;
        double partSourceStart = segment.sourceStart;
        bool first = true;
        for (double splitTime : inside) {
            VideoSegment part = segment;
            if (!first) {
                part.clipKey.itemId = nextId++;
            }
            part.projectStart = partProjectStart;
            part.projectEnd = splitTime;
            part.sourceStart = partSourceStart;
            part.sourceEnd = sourceAtProjectTime(segment, splitTime);
            nextSegments.push_back(std::move(part));

            partProjectStart = splitTime;
            partSourceStart = sourceAtProjectTime(segment, splitTime);
            first = false;
        }

        VideoSegment finalPart = segment;
        finalPart.clipKey.itemId = nextId++;
        finalPart.projectStart = partProjectStart;
        finalPart.sourceStart = partSourceStart;
        nextSegments.push_back(std::move(finalPart));
    }

    link.segments = std::move(nextSegments);
    commitLink(std::move(link), true);
    return true;
}

bool VideoPreviewService::deleteTracks(const au::trackedit::TrackIdList& trackIds)
{
    if (std::find(trackIds.begin(), trackIds.end(), VIDEO_TRACK_ID) == trackIds.end() || !m_link.isValid()) {
        return false;
    }

    // Track deletion is followed by TrackeditOperationController::pushHistoryState().
    // Let that new state capture the deleted link; updating the current state here
    // would make the pre-delete undo state lose the video link too.
    commitLink({}, false);
    return true;
}

bool VideoPreviewService::changeTrackTitle(au::trackedit::TrackId trackId, const muse::String& title)
{
    if (!hasTrack(trackId)) {
        return false;
    }

    VideoLink link = m_link;
    link.trackTitle = title.empty() ? muse::String::fromAscii("Video") : title;
    commitLink(std::move(link), true);
    return true;
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
