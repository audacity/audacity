/*
* Audacity: A Digital Audio Editor
*/
#include "projectviewstate.h"
#include "au3wrap/internal/projectsnap.h"
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
}

ProjectViewState::ProjectViewState(std::shared_ptr<au::au3::IAu3Project> project)
{
    configuration()->setIsEffectsPanelVisible(false);
    qApp->installEventFilter(this);

    m_snap.set(getProjectSnap(project));
    m_snap.ch.onReceive(this, [project = project](const Snap& s) {
        saveProjectSnap(project, s);
    });
}

muse::ValCh<int> ProjectViewState::tracksVericalY() const
{
    return m_tracksVericalY;
}

void ProjectViewState::changeTracksVericalY(int deltaY)
{
    m_tracksVericalY.set(deltaY);
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

int ProjectViewState::trackYPosition(const trackedit::TrackId& trackId) const
{
    trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
    if (!prj) {
        return -1;
    }

    trackedit::TrackIdList tracks = prj->trackIdList();

    int tracksVericalY = this->tracksVericalY().val;
    int trackTop = -tracksVericalY;
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

void ProjectViewState::changeTrackHeight(const trackedit::TrackId& trackId, int deltaY)
{
    TrackData* d = nullptr;
    auto it = m_tracks.find(trackId);
    if (it != m_tracks.end()) {
        d = &it->second;
    } else {
        d = &makeTrackData(trackId);
    }

    int newVal = std::max(d->height.val + deltaY, MIN_HEIGHT);
    d->height.set(newVal);
    d->collapsed.set(newVal < COLLAPSE_HEIGHT);
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

double ProjectViewState::clipEditStartTimeOffset()
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

double ProjectViewState::clipEditEndTimeOffset()
{
    return m_clipEditEndTimeOffset;
}

muse::ValCh<bool> ProjectViewState::altPressed() const
{
    return m_altPressed;
}

muse::ValCh<bool> ProjectViewState::ctrlPressed() const
{
    return m_ctrlPressed;
}

bool ProjectViewState::eventFilter(QObject* watched, QEvent* event)
{
    if (event->type() == QEvent::KeyPress) {
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
