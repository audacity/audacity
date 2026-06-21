/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>
#include <mutex>
#include <optional>
#include <set>
#include <cstdint>
#include <utility>
#include <vector>

#include <QObject>

#include "async/asyncable.h"
#include "context/iglobalcontext.h"
#include "modularity/ioc.h"
#include "trackedit/iauxiliarytrackprovider.h"
#include "trackedit/iclipsinteraction.h"
#include "trackedit/itrackeditproject.h"

#include "../ivideopreviewservice.h"

class AudacityProject;

namespace au::videopreview {

class VideoLinkStorage;
struct VideoDecodeResult;

class VideoPreviewService : public QObject,
                            public IVideoPreviewService,
                            public trackedit::IAuxiliaryTrackProvider,
                            public muse::async::Asyncable,
                            public muse::Contextable,
                            public std::enable_shared_from_this<VideoPreviewService>
{
    Q_OBJECT

    muse::ContextInject<context::IGlobalContext> globalContext{ this };
    muse::ContextInject<trackedit::IClipsInteraction> clipsInteraction{ this };

public:
    explicit VideoPreviewService(const muse::modularity::ContextPtr& ctx);
    ~VideoPreviewService() override;

    void init() override;

    void linkImportedVideo(const muse::io::path_t& sourcePath,
                           const std::vector<au::trackedit::Clip>& clips,
                           double sourceOriginProjectTime) override;

    void setProjectTime(double seconds) override;

    VideoPreviewState state() const override;
    muse::String stateText() const override;
    VideoLink link() const override;
    VideoLinks links() const override;
    muse::io::path_t sourcePath() const override;
    double aspectRatio() const override;
    QImage currentFrame() const override;

    muse::async::Notification frameChanged() const override;
    muse::async::Notification stateChanged() const override;
    muse::async::Notification linkChanged() const override;

    trackedit::TrackIdList trackIdList() const override;
    trackedit::TrackList trackList() const override;
    std::optional<trackedit::Track> track(trackedit::TrackId trackId) const override;
    bool hasTrack(trackedit::TrackId trackId) const override;

    trackedit::Clip clip(const trackedit::ClipKey& key) const override;
    muse::async::NotifyList<trackedit::Clip> clipList(trackedit::TrackId trackId) const override;
    bool hasClip(const trackedit::ClipKey& key) const override;
    trackedit::ClipKeyList clipsOnTrack(trackedit::TrackId trackId) const override;

    std::vector<int64_t> groupsIdsList() const override;
    int64_t clipGroupId(const trackedit::ClipKey& clipKey) const override;
    void setClipGroupId(const trackedit::ClipKey& clipKey, int64_t id) override;
    trackedit::ClipKeyList clipsInGroup(int64_t id) const override;

    trackedit::secs_t totalTime() const override;
    trackedit::secs_t clipStartTime(const trackedit::ClipKey& clipKey) const override;
    trackedit::secs_t clipEndTime(const trackedit::ClipKey& clipKey) const override;
    trackedit::secs_t clipDuration(const trackedit::ClipKey& clipKey) const override;
    bool changeClipStartTime(const trackedit::ClipKey& clipKey, trackedit::secs_t newStartTime, bool completed) override;
    bool changeClipTitle(const trackedit::ClipKey& clipKey, const muse::String& newTitle) override;
    bool changeClipColor(const trackedit::ClipKey& clipKey, trackedit::ClipColorIndex colorIndex) override;

    std::optional<trackedit::TimeSpan> removeClip(const trackedit::ClipKey& clipKey) override;
    bool removeClips(const trackedit::ClipKeyList& clipKeyList, bool moveClips) override;
    muse::RetVal<trackedit::ClipKeyList> moveClips(const trackedit::ClipKeyList& clipKeyList, trackedit::secs_t timePositionOffset,
                                                   int trackPositionOffset, bool completed, bool& clipsMovedToOtherTracks) override;
    bool trimClipsLeft(const trackedit::ClipKeyList& clipKeyList, trackedit::secs_t deltaSec, trackedit::secs_t minClipDuration,
                       bool completed) override;
    bool trimClipsRight(const trackedit::ClipKeyList& clipKeyList, trackedit::secs_t deltaSec, trackedit::secs_t minClipDuration,
                        bool completed) override;
    bool stretchClipsLeft(const trackedit::ClipKeyList& clipKeyList, trackedit::secs_t deltaSec, trackedit::secs_t minClipDuration,
                          bool completed) override;
    bool stretchClipsRight(const trackedit::ClipKeyList& clipKeyList, trackedit::secs_t deltaSec, trackedit::secs_t minClipDuration,
                           bool completed) override;
    bool singleClipOnTrack(trackedit::TrackId trackId) const override;

    bool trimTracksData(const trackedit::TrackIdList& tracksIds, trackedit::secs_t begin, trackedit::secs_t end) override;
    bool removeTracksData(const trackedit::TrackIdList& tracksIds, trackedit::secs_t begin, trackedit::secs_t end, bool moveClips) override;
    bool splitTracksAt(const trackedit::TrackIdList& tracksIds, std::vector<trackedit::secs_t> pivots) override;
    bool deleteTracks(const trackedit::TrackIdList& trackIds) override;
    bool changeTrackTitle(trackedit::TrackId trackId, const muse::String& title) override;

private:
    void onCurrentProjectChanged();
    void detachTrackeditProject();
    void attachTrackeditProject();
    void subscribeToTrack(au::trackedit::TrackId trackId);
    void onStorageLinkChanged(bool probe);

    void setState(VideoPreviewState state);
    void clearFrame();
    void commitLinks(VideoLinks links, bool refreshUndoState);
    void notifyTrackeditAboutLinksChange(const VideoLinks& oldLinks, const VideoLinks& newLinks);
    void notifyTrackeditAboutLinkChange(const VideoLink& oldLink, const VideoLink& newLink);
    trackedit::Track videoTrack(const VideoLink& link) const;
    trackedit::Clips videoClips(const VideoLink& link) const;
    VideoLink normalizedLink(VideoLink link) const;
    VideoLinks normalizedLinks(VideoLinks links) const;
    void updateSegmentMapFromProject(bool refreshUndoState);

    std::vector<au::trackedit::Clip> currentClips() const;
    std::optional<VideoSegment> segmentForClip(const au::trackedit::Clip& clip,
                                               const std::vector<VideoSegment>& sourceSegments) const;
    const VideoLink* linkForTrack(trackedit::TrackId trackId) const;
    VideoLink* linkForTrack(VideoLinks& links, trackedit::TrackId trackId) const;
    const VideoLink* linkForClip(const trackedit::ClipKey& key) const;
    VideoLink* linkForClip(VideoLinks& links, const trackedit::ClipKey& key) const;
    VideoLink activeLink() const;
    std::optional<std::pair<VideoLink, double> > sourceTimeForProjectTime(double projectSeconds) const;

    void requestDecode(VideoLink link, double sourceSeconds);
    void onDecodeFinished(uint64_t generation, VideoDecodeResult result);

    AudacityProject* m_au3Project = nullptr;
    VideoLinkStorage* m_storage = nullptr;
    au::trackedit::ITrackeditProjectPtr m_trackeditProject;
    std::set<au::trackedit::TrackId> m_subscribedTrackIds;

    VideoLinks m_links;
    std::vector<VideoSegment> m_recentSegments;
    VideoPreviewState m_state = VideoPreviewState::Empty;

    mutable std::mutex m_frameMutex;
    mutable muse::async::ChangedNotifier<trackedit::Clip> m_videoClipsChanged;
    QImage m_currentFrame;
    double m_lastProjectTime = 0.0;
    bool m_decodeInFlight = false;
    std::optional<double> m_pendingProjectTime;
    uint64_t m_decodeGeneration = 0;
    bool m_ignoreStorageNotification = false;

    muse::async::Notification m_frameChanged;
    muse::async::Notification m_stateChanged;
    muse::async::Notification m_linkChanged;
};
}
