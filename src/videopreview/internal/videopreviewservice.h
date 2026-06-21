/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>
#include <mutex>
#include <optional>
#include <set>
#include <cstdint>
#include <vector>

#include <QObject>

#include "async/asyncable.h"
#include "context/iglobalcontext.h"
#include "modularity/ioc.h"
#include "trackedit/itrackeditproject.h"

#include "../ivideopreviewservice.h"

class AudacityProject;

namespace au::videopreview {

class VideoLinkStorage;
struct VideoDecodeResult;

class VideoPreviewService : public QObject,
                            public IVideoPreviewService,
                            public muse::async::Asyncable,
                            public muse::Contextable,
                            public std::enable_shared_from_this<VideoPreviewService>
{
    Q_OBJECT

    muse::ContextInject<context::IGlobalContext> globalContext{ this };

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
    muse::io::path_t sourcePath() const override;
    double aspectRatio() const override;
    QImage currentFrame() const override;

    muse::async::Notification frameChanged() const override;
    muse::async::Notification stateChanged() const override;
    muse::async::Notification linkChanged() const override;

private:
    void onCurrentProjectChanged();
    void detachTrackeditProject();
    void attachTrackeditProject();
    void subscribeToTrack(au::trackedit::TrackId trackId);
    void onStorageLinkChanged(bool probe);

    void setState(VideoPreviewState state);
    void clearFrame();
    void commitLink(VideoLink link, bool refreshUndoState);
    void updateSegmentMapFromProject(bool refreshUndoState);

    std::vector<au::trackedit::Clip> currentClips() const;
    std::optional<VideoSegment> segmentForClip(const au::trackedit::Clip& clip,
                                               const std::vector<VideoSegment>& sourceSegments) const;
    std::optional<double> sourceTimeForProjectTime(double projectSeconds) const;

    void requestDecode(double sourceSeconds);
    void onDecodeFinished(uint64_t generation, VideoDecodeResult result);

    AudacityProject* m_au3Project = nullptr;
    VideoLinkStorage* m_storage = nullptr;
    au::trackedit::ITrackeditProjectPtr m_trackeditProject;
    std::set<au::trackedit::TrackId> m_subscribedTrackIds;

    VideoLink m_link;
    std::vector<VideoSegment> m_recentSegments;
    VideoPreviewState m_state = VideoPreviewState::Empty;

    mutable std::mutex m_frameMutex;
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
