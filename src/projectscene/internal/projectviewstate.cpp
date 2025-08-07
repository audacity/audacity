/*
* Audacity: A Digital Audio Editor
*/
#include "projectviewstate.h"
#include "au3wrap/internal/projectsnap.h"
#include "au3/viewinfo.h"
#include "au3wrap/au3types.h"

using namespace au::projectscene;

constexpr int DEFAULT_HEIGHT = 116;
constexpr int MIN_HEIGHT = 44;
constexpr int COLLAPSE_HEIGHT = 72;

namespace {
void saveProjectSnap(std::shared_ptr<au::au3::IAu3Project> project, const Snap& snap)
{
    au::au3::Au3Project* au3Project = reinterpret_cast<au::au3::Au3Project*>(project->au3ProjectPtr());
    IF_ASSERT_FAILED(au3Project) {
        return;
    }
    auto& projectSnap = au::au3::ProjectSnap::Get(*au3Project);
    projectSnap.setSnapType(static_cast<unsigned int>(snap.type));
    projectSnap.enableSnap(snap.enabled);
    projectSnap.setSnapTriplets(snap.isSnapTriplets);
}

Snap getProjectSnap(std::shared_ptr<au::au3::IAu3Project> project)
{
    au::au3::Au3Project* au3Project = reinterpret_cast<au::au3::Au3Project*>(project->au3ProjectPtr());
    IF_ASSERT_FAILED(au3Project) {
        return Snap {};
    }
    auto& projectSnap = au::au3::ProjectSnap::Get(*au3Project);
    return Snap {
        static_cast<SnapType>(projectSnap.snapType()),
        projectSnap.isSnapEnabled(),
        projectSnap.isSnapTriplets()
    };
}

void saveProjectZoomState(au::au3::Au3Project* au3Project, const ZoomState& zoomState)
{
    auto& projectZoomState = au::au3::ViewInfo::Get(*au3Project);
    projectZoomState.setZoom(zoomState.zoom);
    projectZoomState.setVPos(zoomState.tracksVerticalOffset);
    projectZoomState.setHPos(zoomState.frameStart);
}

ZoomState getProjectZoomState(au::au3::Au3Project* au3Project)
{
    auto& projectZoomState = au::au3::ViewInfo::Get(*au3Project);
    return ZoomState {
        projectZoomState.zoom(),
        projectZoomState.hPos(),
        projectZoomState.vPos()
    };
}
}

ProjectViewState::ProjectViewState(std::shared_ptr<au::au3::IAu3Project> project)
{
    configuration()->setIsEffectsPanelVisible(false);
    qApp->installEventFilter(this);

    m_snap.set(getProjectSnap(project));
    m_snap.ch.onReceive(this, [project = project](const Snap& s) {
        saveProjectSnap(project, s);
    });

    au::au3::Au3Project* au3Project = reinterpret_cast<au::au3::Au3Project*>(project->au3ProjectPtr());
    IF_ASSERT_FAILED(au3Project) {
        return;
    }

    m_tracksVerticalOffset.set(getProjectZoomState(au3Project).tracksVerticalOffset);
    m_tracksVerticalOffset.ch.onReceive(this, [au3Project](const int y) {
        ZoomState zoomState = getProjectZoomState(au3Project);
        zoomState.tracksVerticalOffset = y;
        saveProjectZoomState(au3Project, zoomState);
    });

    globalContext()->currentTrackeditProjectChanged().onNotify(this, [this](){
        auto prj = globalContext()->currentTrackeditProject();
        if (!prj) {
            return;
        }

        prj->trackRemoved().onReceive(this, [this](const trackedit::Track& track) {
            auto it = m_tracks.find(track.id);
            if (it == m_tracks.end()) {
                return;
            }
            m_totalTracksHeight.set(m_totalTracksHeight.val - it->second.height.val);
            m_tracks.erase(it);
        });
    });
}

muse::ValCh<int> ProjectViewState::totalTrackHeight() const
{
    return m_totalTracksHeight;
}

muse::ValCh<int> ProjectViewState::tracksVerticalOffset() const
{
    return m_tracksVerticalOffset;
}

void ProjectViewState::changeTracksVerticalOffset(int deltaY)
{
    m_tracksVerticalOffset.set(deltaY);
}

double ProjectViewState::mousePositionY() const
{
    return m_mouseYPosition.val;
}

void ProjectViewState::setMousePositionY(double y)
{
    m_mouseYPosition.set(y);
}

muse::ValCh<bool> ProjectViewState::tracksVerticalScrollLocked() const
{
    return m_tracksVerticalScrollLocked;
}

void ProjectViewState::setTracksVerticalScrollLocked(bool lock)
{
    m_tracksVerticalScrollLocked.set(lock);
}

int ProjectViewState::trackVerticalPosition(const trackedit::TrackId& trackId) const
{
    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return -1;
    }

    trackedit::TrackIdList tracks = prj->trackIdList();

    int tracksVerticalOffset = this->tracksVerticalOffset().val;
    int trackTop = -tracksVerticalOffset;
    int trackBottom = trackTop;

    for (trackedit::TrackId id : tracks) {
        trackTop = trackBottom;
        trackBottom = trackTop + trackHeight(id).val;

        if (trackId == id) {
            return trackTop;
        }
    }

    return -1;
}

ProjectViewState::TrackData& ProjectViewState::makeTrackData(const trackedit::TrackId& trackId) const
{
    TrackData d;
    d.height.val = DEFAULT_HEIGHT;
    d.collapsed.val = false;
    d.channelHeightRatio.val = 1.0;
    m_totalTracksHeight.set(m_totalTracksHeight.val + d.height.val);

    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (prj) {
        std::optional<trackedit::Track> track = prj->track(trackId);
        if (track) {
            d.channelHeightRatio.val = track->type == trackedit::TrackType::Stereo ? 0.5 : 1.0;
        }
    }
    return m_tracks.insert({ trackId, d }).first->second;
}

muse::ValCh<int> ProjectViewState::trackHeight(const trackedit::TrackId& trackId) const
{
    auto it = m_tracks.find(trackId);
    if (it != m_tracks.end()) {
        return it->second.height;
    }

    const ProjectViewState::TrackData& d = makeTrackData(trackId);
    return d.height;
}

muse::ValCh<bool> ProjectViewState::isTrackCollapsed(const trackedit::TrackId& trackId) const
{
    auto it = m_tracks.find(trackId);
    if (it != m_tracks.end()) {
        return it->second.collapsed;
    }

    const ProjectViewState::TrackData& d = makeTrackData(trackId);
    return d.collapsed;
}

muse::ValCh<double> ProjectViewState::channelHeightRatio(const trackedit::TrackId& trackId) const
{
    auto it = m_tracks.find(trackId);
    if (it != m_tracks.end()) {
        return it->second.channelHeightRatio;
    }

    const ProjectViewState::TrackData& d = makeTrackData(trackId);
    return d.channelHeightRatio;
}

void ProjectViewState::changeTrackHeight(const trackedit::TrackId& trackId, int delta)
{
    TrackData* d = nullptr;
    auto it = m_tracks.find(trackId);
    if (it != m_tracks.end()) {
        d = &it->second;
    } else {
        d = &makeTrackData(trackId);
    }

    int oldHeight = d->height.val;
    int newHeight = std::max(oldHeight + delta, MIN_HEIGHT);

    d->height.set(newHeight);
    d->collapsed.set(newHeight < COLLAPSE_HEIGHT);

    m_totalTracksHeight.set(m_totalTracksHeight.val + (newHeight - oldHeight));
}

void ProjectViewState::setTrackHeight(const trackedit::TrackId& trackId, int height)
{
    TrackData* d = nullptr;
    auto it = m_tracks.find(trackId);
    if (it != m_tracks.end()) {
        d = &it->second;
    } else {
        d = &makeTrackData(trackId);
    }

    int oldHeight = d->height.val;
    int newHeight = std::max(height, MIN_HEIGHT);
    d->height.set(newHeight);
    d->collapsed.set(height < COLLAPSE_HEIGHT);

    m_totalTracksHeight.set(m_totalTracksHeight.val + (newHeight - oldHeight));
}

au::trackedit::TrackId ProjectViewState::trackAtPosition(double y) const
{
    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return trackedit::INVALID_TRACK;
    }

    trackedit::TrackIdList tracks = prj->trackIdList();

    int tracksVerticalOffset = this->tracksVerticalOffset().val;
    int trackTop = -tracksVerticalOffset;
    int trackBottom = trackTop;

    for (trackedit::TrackId id : tracks) {
        trackTop = trackBottom;
        trackBottom = trackTop + trackHeight(id).val;

        if (muse::RealIsEqualOrMore(y, trackTop) && muse::RealIsEqualOrLess(y, trackBottom)) {
            return id;
        }
    }

    return trackedit::INVALID_TRACK;
}

au::trackedit::TrackIdList ProjectViewState::tracksInRange(double y1, double y2) const
{
    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return {};
    }

    if (y1 < 0 && y2 < 0) {
        return {};
    }

    if (y1 > y2) {
        std::swap(y1, y2);
    }

    if (y1 < 1) {
        y1 = 1;
    }

    trackedit::TrackIdList tracks = prj->trackIdList();
    trackedit::TrackIdList result;

    int trackTop = -m_tracksVerticalOffset.val;
    int trackBottom = trackTop;

    for (trackedit::TrackId trackId : tracks) {
        trackTop = trackBottom;
        trackBottom = trackTop + trackHeight(trackId).val;

        if (muse::RealIsEqualOrMore(y1, trackTop) && !muse::RealIsEqualOrMore(y1, trackBottom)) {
            result.push_back(trackId);
        }

        if (muse::RealIsEqualOrMore(y2, trackTop) && !muse::RealIsEqualOrMore(y2, trackBottom)) {
            if (!result.empty() && result.back() != trackId) {
                result.push_back(trackId);
            }
            break;
        }

        if (!result.empty() && result.back() != trackId) {
            result.push_back(trackId);
            continue;
        }
    }

    return result;
}

void ProjectViewState::setChannelHeightRatio(const trackedit::TrackId& trackId, double ratio)
{
    TrackData* d = nullptr;
    auto it = m_tracks.find(trackId);
    if (it != m_tracks.end()) {
        d = &it->second;
    } else {
        d = &makeTrackData(trackId);
    }

    d->channelHeightRatio.set(ratio);
}

bool ProjectViewState::isSnapEnabled() const
{
    return m_snap.val.enabled;
}

void ProjectViewState::setIsSnapEnabled(bool enabled)
{
    Snap s = this->snap().val;
    s.enabled = enabled;
    setSnap(s);
}

SnapType ProjectViewState::snapType() const
{
    return m_snap.val.type;
}

void ProjectViewState::setSnapType(SnapType type)
{
    Snap s = this->snap().val;
    s.type = type;
    setSnap(s);
}

bool ProjectViewState::isSnapTripletsEnabled() const
{
    return m_snap.val.isSnapTriplets;
}

void ProjectViewState::setIsSnapTripletsEnabled(bool enabled)
{
    Snap s = this->snap().val;
    s.isSnapTriplets = enabled;
    setSnap(s);
}

void ProjectViewState::setSnap(const Snap& s)
{
    m_snap.set(s);
}

Snap ProjectViewState::getSnap() const
{
    return m_snap.val;
}

muse::ValCh<Snap> ProjectViewState::snap() const
{
    return m_snap;
}

void ProjectViewState::setClipEditStartTimeOffset(double val)
{
    if (m_clipEditStartTimeOffset == val) {
        return;
    }

    m_clipEditStartTimeOffset = val;
}

double ProjectViewState::clipEditStartTimeOffset() const
{
    return m_clipEditStartTimeOffset;
}

void ProjectViewState::setClipEditEndTimeOffset(double val)
{
    if (m_clipEditEndTimeOffset == val) {
        return;
    }

    m_clipEditEndTimeOffset = val;
}

double ProjectViewState::clipEditEndTimeOffset() const
{
    return m_clipEditEndTimeOffset;
}

void ProjectViewState::setMoveInitiated(bool val)
{
    if (m_moveInitiated == val) {
        return;
    }

    m_moveInitiated = val;
}

bool ProjectViewState::moveInitiated() const
{
    return m_moveInitiated;
}

void ProjectViewState::setLastEditedClip(const trackedit::ClipKey& clipKey)
{
    if (m_lastEditedClip == clipKey) {
        return;
    }

    m_lastEditedClip = clipKey;
}

au::trackedit::ClipKey ProjectViewState::lastEditedClip() const
{
    return m_lastEditedClip;
}

void ProjectViewState::setClipsBoundaries(const std::set<muse::secs_t>& boundaries)
{
    m_clipsBoundaries = boundaries;
}

std::set<muse::secs_t> ProjectViewState::clipsBoundaries() const
{
    return m_clipsBoundaries;
}

void ProjectViewState::updateClipsBoundaries(bool excludeCurrentSelection, const trackedit::ClipKey& keyToOmit)
{
    auto prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return;
    }

    std::set<muse::secs_t> boundaries;
    for (const auto& trackId : prj->trackIdList()) {
        for (const auto& clip : prj->clipList(trackId)) {
            if (excludeCurrentSelection && muse::contains(selectionController()->selectedClips(), clip.key)) {
                continue;
            }

            if (keyToOmit.isValid() && clip.key == keyToOmit) {
                continue;
            }

            boundaries.insert(clip.startTime);
            boundaries.insert(clip.endTime);
        }
    }

    setClipsBoundaries(boundaries);
}

void ProjectViewState::setZoomState(const ZoomState& state)
{
    au::au3::Au3Project* project = reinterpret_cast<au::au3::Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
    saveProjectZoomState(project, state);
}

ZoomState ProjectViewState::zoomState() const
{
    au::au3::Au3Project* project = reinterpret_cast<au::au3::Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
    return getProjectZoomState(project);
}

muse::ValCh<bool> ProjectViewState::altPressed() const
{
    return m_altPressed;
}

muse::ValCh<bool> ProjectViewState::ctrlPressed() const
{
    return m_ctrlPressed;
}

muse::ValCh<bool> ProjectViewState::escPressed() const
{
    return m_escPressed;
}

int ProjectViewState::trackDefaultHeight() const
{
    return DEFAULT_HEIGHT;
}

bool ProjectViewState::eventFilter(QObject* watched, QEvent* event)
{
    if (event->type() == QEvent::KeyPress || event->type() == QEvent::ShortcutOverride) {
        if (static_cast<QKeyEvent*>(event)->key() == 0 || static_cast<QKeyEvent*>(event)->key() == Qt::Key_unknown) {
            return QObject::eventFilter(watched, event);
        }

        if (static_cast<QKeyEvent*>(event)->key() == Qt::Key_Alt
            && static_cast<QKeyEvent*>(event)->modifiers() == Qt::AltModifier) {
            if (!m_altPressed.val) {
                m_altPressed.set(true);
            }
        } else if (static_cast<QKeyEvent*>(event)->key() == Qt::Key_Control
                   && static_cast<QKeyEvent*>(event)->modifiers() == Qt::ControlModifier) {
            if (!m_ctrlPressed.val) {
                m_ctrlPressed.set(true);
            }
        } else if (static_cast<QKeyEvent*>(event)->key() == Qt::Key_Escape) {
            if (!m_escPressed.val) {
                m_escPressed.set(true);
            }
        } else {
            // We only want to process single ALT and CTRL key presses
            if (m_altPressed.val) {
                m_altPressed.set(false);
            }

            if (m_ctrlPressed.val) {
                m_ctrlPressed.set(false);
            }

            if (m_escPressed.val) {
                m_escPressed.set(false);
            }
        }
    } else if (event->type() == QEvent::KeyRelease) {
        if (static_cast<QKeyEvent*>(event)->key() == 0 || static_cast<QKeyEvent*>(event)->key() == Qt::Key_unknown) {
            return QObject::eventFilter(watched, event);
        }

        if (static_cast<QKeyEvent*>(event)->key() == Qt::Key_Alt || (static_cast<QKeyEvent*>(event)->modifiers() & Qt::AltModifier)) {
            m_altPressed.set(false);
        }

        if (static_cast<QKeyEvent*>(event)->key() == Qt::Key_Control
            || (static_cast<QKeyEvent*>(event)->modifiers() & Qt::ControlModifier)) {
            m_ctrlPressed.set(false);
        }

        if (m_escPressed.val) {
            m_escPressed.set(false);
        }
    } else if (event->type() == QEvent::ApplicationStateChange) {
        if (qApp->applicationState() != Qt::ApplicationState::ApplicationActive) {
            if (m_altPressed.val) {
                m_altPressed.set(false);
            }

            if (m_ctrlPressed.val) {
                m_ctrlPressed.set(false);
            }

            if (m_escPressed.val) {
                m_escPressed.set(false);
            }
        }
    }

    return QObject::eventFilter(watched, event);
}
