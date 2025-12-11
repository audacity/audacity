/*
* Audacity: A Digital Audio Editor
*/

#include <memory>

#include "framework/global/types/retval.h"

#include "au3wrap/iau3project.h"
#include "au3wrap/internal/projectsnap.h"
#include "au3wrap/internal/domaccessor.h"
#include "au3wrap/internal/domconverter.h"
#include "au3wrap/au3types.h"
#include "trackedit/dom/track.h"

#include "au3/waveformscale.h"
#include "au3/viewinfo.h"
#include "au3/trackrulertypeattachment.h"

#include "projectviewstate.h"

using namespace au::projectscene;

constexpr int TRACK_DEFAULT_HEIGHT = 116;
constexpr int TRACK_LABEL_DEFAULT_HEIGHT = 86;
constexpr int TRACK_MIN_HEIGHT = 44;
constexpr int TRACK_COLLAPSE_HEIGHT = 72;

namespace {
constexpr int numDecimals(float value)
{
    constexpr int MAX_DECIMAL_PLACES = 4;
    constexpr float POWER_OF_TEN = 10000.0f;

    int rounded_scaled_value = static_cast<int>((value * POWER_OF_TEN) + 0.5f);
    int integer_part = static_cast<int>(value);
    int decimal_part_scaled = rounded_scaled_value - (integer_part * static_cast<int>(POWER_OF_TEN));

    if (decimal_part_scaled == 0) {
        return 0;
    }

    int count = MAX_DECIMAL_PLACES;
    int power_of_ten = 10;

    while (count > 0 && (decimal_part_scaled % power_of_ten) == 0) {
        count--;
        power_of_ten *= 10;
    }

    return count;
}

static_assert(numDecimals(0.14999999) == 2);
static_assert(numDecimals(1.00) == 0);
static_assert(numDecimals(0.005) == 3);

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

std::pair<float, float> getVerticalDisplayBounds(std::shared_ptr<au::project::IAudacityProject> project,
                                                 const au::trackedit::TrackId& trackId)
{
    au::au3::Au3Project* au3Project = reinterpret_cast<au::au3::Au3Project*>(project->au3ProjectPtr());

    au::au3::Au3WaveTrack* waveTrack = au::au3::DomAccessor::findWaveTrack(*au3Project, au::au3::Au3TrackId(trackId));
    if (waveTrack == nullptr) {
        return { -1.0f, 1.0f };
    }

    float min = 0.0f;
    float max = 0.0f;
    auto& cache = WaveformScale::Get(*waveTrack);
    cache.GetDisplayBounds(min, max);
    return { min, max };
}

void setVerticalDisplayBounds(std::shared_ptr<au::project::IAudacityProject> project, const au::trackedit::TrackId& trackId,
                              const std::pair<float, float>& bounds)
{
    au::au3::Au3Project* au3Project = reinterpret_cast<au::au3::Au3Project*>(project->au3ProjectPtr());

    au::au3::Au3WaveTrack* waveTrack = au::au3::DomAccessor::findWaveTrack(*au3Project, au::au3::Au3TrackId(trackId));
    if (waveTrack == nullptr) {
        return;
    }

    auto& cache = WaveformScale::Get(*waveTrack);
    cache.SetDisplayBounds(bounds.first, bounds.second);
}

int getTrackRulerType(std::shared_ptr<au::project::IAudacityProject> project, const au::trackedit::TrackId& trackId)
{
    au::au3::Au3Project* au3Project = reinterpret_cast<au::au3::Au3Project*>(project->au3ProjectPtr());

    au::au3::Au3Track* track = au::au3::DomAccessor::findTrack(*au3Project, au::au3::Au3TrackId(trackId));
    if (track == nullptr) {
        return static_cast<int>(au::trackedit::TrackRulerType::Linear);
    }

    auto& cache = TrackRulerTypeAttachment::Get(track);
    return static_cast<int>(cache.GetRulerType());
}

void setTrackRulerType(std::shared_ptr<au::project::IAudacityProject> project, const au::trackedit::TrackId& trackId, int rulerType)
{
    au::au3::Au3Project* au3Project = reinterpret_cast<au::au3::Au3Project*>(project->au3ProjectPtr());

    au::au3::Au3Track* track = au::au3::DomAccessor::findTrack(*au3Project, au::au3::Au3TrackId(trackId));
    if (track == nullptr) {
        return;
    }

    auto& cache = TrackRulerTypeAttachment::Get(track);
    cache.SetRulerType(static_cast<au::trackedit::TrackRulerType>(rulerType));
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

    au3::setViewStateRestorerNotification(*au3Project, m_rolledBack);

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
            m_verticalRulerWidth.set(calculateVerticalRulerWidth());
            m_tracks.erase(it);
        });

        updateItemsBoundaries(false);
        prj->trackChanged().onReceive(this, [this](const trackedit::Track&) {
            updateItemsBoundaries(false);
        });

        prj->trackAdded().onReceive(this, [this](const trackedit::Track&) {
            updateItemsBoundaries(false);
        });

        prj->trackInserted().onReceive(this, [this](const trackedit::Track&, int) {
            updateItemsBoundaries(false);
        });
    });

    projectHistory()->historyChanged().onNotify(this, [this]() {
        updateItemsBoundaries(false);
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
    const project::IAudacityProjectPtr prj = globalContext()->currentProject();

    const std::pair<float, float> defaultBounds = getVerticalDisplayBounds(prj, trackId);
    const int defaultRulerType = getTrackRulerType(prj, trackId);

    TrackData d;
    d.collapsed.val = false;
    d.channelHeightRatio.val = 1.0;
    d.verticalDisplayBounds.val = defaultBounds;
    d.isHalfWave.val = (defaultBounds.first == 0.0f);
    d.rulerType.val = defaultRulerType;

    trackedit::ITrackeditProjectPtr trackeditPrj = globalContext()->currentTrackeditProject();
    if (trackeditPrj) {
        std::optional<trackedit::Track> track = trackeditPrj->track(trackId);
        if (track) {
            d.channelHeightRatio.val = track->type == trackedit::TrackType::Stereo ? 0.5 : 1.0;
            d.height.val = track->type == trackedit::TrackType::Label ? TRACK_LABEL_DEFAULT_HEIGHT : TRACK_DEFAULT_HEIGHT;
        }
    }

    m_totalTracksHeight.set(m_totalTracksHeight.val + d.height.val);
    m_verticalRulerWidth.set(calculateVerticalRulerWidth());

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
    int newHeight = std::max(oldHeight + delta, TRACK_MIN_HEIGHT);

    d->height.set(newHeight);
    d->collapsed.set(newHeight < TRACK_COLLAPSE_HEIGHT);

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
    int newHeight = std::max(height, TRACK_MIN_HEIGHT);
    d->height.set(newHeight);
    d->collapsed.set(height < TRACK_COLLAPSE_HEIGHT);

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

void ProjectViewState::setSplitToolEnabled(const bool enabled)
{
    if (m_splitToolEnabled.val == enabled) {
        return;
    }

    m_splitToolEnabled.set(enabled);
}

muse::ValCh<bool> ProjectViewState::splitToolEnabled()
{
    return m_splitToolEnabled;
}

muse::ValCh<std::pair<float, float> > ProjectViewState::verticalDisplayBounds(const trackedit::TrackId& trackId) const
{
    auto it = m_tracks.find(trackId);
    if (it != m_tracks.end()) {
        return it->second.verticalDisplayBounds;
    }

    const ProjectViewState::TrackData& d = makeTrackData(trackId);
    return d.verticalDisplayBounds;
}

float ProjectViewState::maxVerticalZoomLevel(const trackedit::TrackId& trackId) const
{
    constexpr int MIN_DISTANCE_FROM_RANGE = 6;
    constexpr int DB_PER_STEP = 6;
    constexpr float MIN_VERTICAL_RANGE = 1.0f / (1 << 9);

    const auto project = globalContext()->currentProject();
    if (!project) {
        return MIN_VERTICAL_RANGE;
    }

    const au::au3::Au3Project* au3Project = reinterpret_cast<const au::au3::Au3Project*>(project->au3ProjectPtr());

    const au::au3::Au3WaveTrack* waveTrack = au::au3::DomAccessor::findWaveTrack(*au3Project, ::TrackId(trackId));
    if (waveTrack == nullptr) {
        return MIN_VERTICAL_RANGE;
    }

    trackedit::Track track = au::au3::DomConverter::track(waveTrack);

    const auto rulerType = static_cast<trackedit::TrackRulerType>(::getTrackRulerType(project, trackId));

    if (rulerType == trackedit::TrackRulerType::DbLog) {
        const int dBRange = static_cast<int>(playback::PlaybackMeterDbRange::toDouble(playbackConfiguration()->playbackMeterDbRange()));
        const int steps = (-dBRange - MIN_DISTANCE_FROM_RANGE) / DB_PER_STEP;
        return std::max(MIN_VERTICAL_RANGE, 1.0f / (1 << steps));
    }

    return MIN_VERTICAL_RANGE;
}

void ProjectViewState::zoomInVertically(const trackedit::TrackId& trackId)
{
    const auto project = globalContext()->currentProject();
    if (!project) {
        return;
    }

    const auto& [verticalMin, verticalMax] = getVerticalDisplayBounds(project, trackId);
    const float maxZoom = maxVerticalZoomLevel(trackId);

    auto it = m_tracks.find(trackId);
    if (it != m_tracks.end()) {
        const std::pair<float, float> newBounds = {
            std::min(verticalMin / 2.0f, -maxZoom),
            std::max(verticalMax / 2.0f, maxZoom)
        };
        setVerticalDisplayBounds(project, trackId, newBounds);
        it->second.verticalDisplayBounds.set(newBounds);
        m_verticalRulerWidth.set(calculateVerticalRulerWidth());
    }
}

void ProjectViewState::zoomOutVertically(const trackedit::TrackId& trackId)
{
    constexpr float MAX_VERTICAL_RANGE = 2.0f;

    const auto project = globalContext()->currentProject();
    if (!project) {
        return;
    }

    const auto& [verticalMin, verticalMax] = getVerticalDisplayBounds(project, trackId);

    auto it = m_tracks.find(trackId);
    if (it != m_tracks.end()) {
        const std::pair<float, float> newBounds = {
            std::max(verticalMin * 2.0f, -MAX_VERTICAL_RANGE),
            std::min(verticalMax * 2.0f, MAX_VERTICAL_RANGE)
        };
        setVerticalDisplayBounds(project, trackId, newBounds);
        it->second.verticalDisplayBounds.set(newBounds);
        m_verticalRulerWidth.set(calculateVerticalRulerWidth());
    }
}

void ProjectViewState::resetVerticalZoom(const trackedit::TrackId& trackId)
{
    const float DEFAULT_VERTICAL_MIN = -1.0f;
    const float DEFAULT_VERTICAL_MAX = 1.0f;

    const auto project = globalContext()->currentProject();
    if (!project) {
        return;
    }

    const auto& [verticalMin, _] = getVerticalDisplayBounds(project, trackId);
    const bool isHalfWave = (verticalMin == 0.0f);

    const std::pair<float, float> defaultBounds =  { isHalfWave ? 0.0f : DEFAULT_VERTICAL_MIN, DEFAULT_VERTICAL_MAX };

    auto it = m_tracks.find(trackId);
    if (it != m_tracks.end()) {
        setVerticalDisplayBounds(project, trackId, defaultBounds);
        it->second.verticalDisplayBounds.set(defaultBounds);
        m_verticalRulerWidth.set(calculateVerticalRulerWidth());
    }
}

bool ProjectViewState::isDefaultVerticalZoom(const trackedit::TrackId& trackId) const
{
    constexpr float DEFAULT_VERTICAL_MAX = 1.0f;

    const auto project = globalContext()->currentProject();
    if (!project) {
        return false;
    }

    const auto& [_, verticalMax] = getVerticalDisplayBounds(project, trackId);
    return muse::RealIsEqual(verticalMax, DEFAULT_VERTICAL_MAX);
}

bool ProjectViewState::isMaxVerticalZoom(const trackedit::TrackId& trackId) const
{
    const auto project = globalContext()->currentProject();
    if (!project) {
        return false;
    }

    const auto& [_, verticalMax] = getVerticalDisplayBounds(project, trackId);
    const float maxZoom = maxVerticalZoomLevel(trackId);
    return muse::RealIsEqualOrLess(std::abs(verticalMax), maxZoom);
}

bool ProjectViewState::isMinVerticalZoom(const trackedit::TrackId& trackId) const
{
    constexpr float MAX_VERTICAL_RANGE = 2.0f;

    const auto project = globalContext()->currentProject();
    if (!project) {
        return false;
    }

    const auto& [_, verticalMax] = getVerticalDisplayBounds(project, trackId);
    return muse::RealIsEqualOrMore(std::abs(verticalMax), MAX_VERTICAL_RANGE);
}

muse::ValCh<bool> ProjectViewState::isHalfWave(const trackedit::TrackId& trackId) const
{
    auto it = m_tracks.find(trackId);
    if (it != m_tracks.end()) {
        return it->second.isHalfWave;
    }

    const ProjectViewState::TrackData& d = makeTrackData(trackId);
    return d.isHalfWave;
}

void ProjectViewState::toggleHalfWave(const trackedit::TrackId& trackId)
{
    const auto project = globalContext()->currentProject();
    if (!project) {
        return;
    }

    const auto& [verticalMin, verticalMax] = getVerticalDisplayBounds(project, trackId);

    auto it = m_tracks.find(trackId);
    if (it != m_tracks.end()) {
        bool isHalfWave = (verticalMin == 0.0f);
        setVerticalDisplayBounds(project, trackId,
                                 { isHalfWave ? -verticalMax : 0.0f, verticalMax });
        it->second.verticalDisplayBounds.set(
            { isHalfWave ? -verticalMax : 0.0f, verticalMax });
        it->second.isHalfWave.set(!isHalfWave);
        m_verticalRulerWidth.set(calculateVerticalRulerWidth());
    }
}

muse::ValCh<int> ProjectViewState::trackRulerType(const trackedit::TrackId& trackId) const
{
    auto it = m_tracks.find(trackId);
    if (it != m_tracks.end()) {
        return it->second.rulerType;
    }

    const ProjectViewState::TrackData& d = makeTrackData(trackId);
    return d.rulerType;
}

void ProjectViewState::setTrackRulerType(const trackedit::TrackId& trackId, int rulerType)
{
    const auto project = globalContext()->currentProject();
    if (!project) {
        return;
    }

    auto it = m_tracks.find(trackId);
    if (it != m_tracks.end()) {
        it->second.rulerType.set(rulerType);
        ::setTrackRulerType(project, trackId, rulerType);
        m_verticalRulerWidth.set(calculateVerticalRulerWidth());
    }
}

muse::ValCh<int> ProjectViewState::verticalRulerWidth() const
{
    return m_verticalRulerWidth;
}

void ProjectViewState::setItemEditStartTimeOffset(double val)
{
    if (m_itemEditStartTimeOffset == val) {
        return;
    }

    m_itemEditStartTimeOffset = val;
}

double ProjectViewState::itemEditStartTimeOffset() const
{
    return m_itemEditStartTimeOffset;
}

void ProjectViewState::setItemEditEndTimeOffset(double val)
{
    if (m_itemEditEndTimeOffset == val) {
        return;
    }

    m_itemEditEndTimeOffset = val;
}

double ProjectViewState::itemEditEndTimeOffset() const
{
    return m_itemEditEndTimeOffset;
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

void ProjectViewState::setItemsBoundaries(const std::set<muse::secs_t>& boundaries)
{
    m_itemsBoundaries = boundaries;
}

std::set<muse::secs_t> ProjectViewState::itemsBoundaries() const
{
    return m_itemsBoundaries;
}

void ProjectViewState::updateItemsBoundaries(bool excludeCurrentSelection, const trackedit::TrackItemKey& itemKeyToOmit)
{
    auto prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return;
    }

    std::set<muse::secs_t> boundaries;
    const auto& selectedClips = selectionController()->selectedClips();
    const auto& selectedLabels = selectionController()->selectedLabels();

    if (selectedClips.empty() && selectedLabels.empty()) {
        setItemsBoundaries({});
        return;
    }

    for (const auto& trackId : prj->trackIdList()) {
        // Add clips boundaries
        for (const auto& clip : prj->clipList(trackId)) {
            if (excludeCurrentSelection && muse::contains(selectedClips, clip.key)) {
                continue;
            }

            if (itemKeyToOmit.isValid() && clip.key == itemKeyToOmit) {
                continue;
            }

            boundaries.insert(clip.startTime);
            boundaries.insert(clip.endTime);
        }

        // Add labels boundaries
        for (const auto& label : prj->labelList(trackId)) {
            boundaries.insert(label.startTime);
            boundaries.insert(label.endTime);
        }
    }

    setItemsBoundaries(boundaries);
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

muse::async::Notification ProjectViewState::rolledBack() const
{
    return m_rolledBack;
}

muse::ValCh<bool> ProjectViewState::altPressed() const
{
    return m_altPressed;
}

muse::ValCh<bool> ProjectViewState::ctrlPressed() const
{
    return m_ctrlPressed;
}

int ProjectViewState::trackDefaultHeight() const
{
    return TRACK_DEFAULT_HEIGHT;
}

int ProjectViewState::calculateVerticalRulerWidth() const
{
    constexpr int MIN_RULER_WIDTH = 56;

    const auto project = globalContext()->currentProject();
    if (!project) {
        return MIN_RULER_WIDTH;
    }

    float smallestBoundValue = 1;
    for (const auto& [trackId, trackData] : m_tracks) {
        const auto rulerType = static_cast<au::trackedit::TrackRulerType>(::getTrackRulerType(project, trackId));
        if (rulerType != au::trackedit::TrackRulerType::Linear) {
            continue;
        }

        const auto& [minBound, maxBound] = verticalDisplayBounds(trackId).val;

        // Find the smallest vertical zoom among linear rulers to ajust the width globally.
        smallestBoundValue = std::min(smallestBoundValue, ((maxBound - minBound) / 2.0f));
    }
    smallestBoundValue *= 0.1f;

    const int nDecimals = numDecimals(smallestBoundValue);

    // Adjust the width according to the number of decimal places.
    return MIN_RULER_WIDTH + (nDecimals * 8);
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
        } else {
            // We only want to process single ALT and CTRL key presses
            if (m_altPressed.val) {
                m_altPressed.set(false);
            }

            if (m_ctrlPressed.val) {
                m_ctrlPressed.set(false);
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
    } else if (event->type() == QEvent::ApplicationStateChange) {
        if (qApp->applicationState() != Qt::ApplicationState::ApplicationActive) {
            if (m_altPressed.val) {
                m_altPressed.set(false);
            }

            if (m_ctrlPressed.val) {
                m_ctrlPressed.set(false);
            }
        }
    }

    return QObject::eventFilter(watched, event);
}
