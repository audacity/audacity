/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../iprojectviewstate.h"
#include "../iprojectsceneconfiguration.h"

#include "context/iglobalcontext.h"
#include "modularity/ioc.h"
#include "async/asyncable.h"
#include "au3wrap/iau3project.h"

namespace au::projectscene {
class ProjectViewState : public QObject, public IProjectViewState, public muse::async::Asyncable
{
    Q_OBJECT

    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<IProjectSceneConfiguration> configuration;

public:
    ProjectViewState(std::shared_ptr<au::au3::IAu3Project> project);

    // State of elements
    muse::ValCh<int> trackHeight(const trackedit::TrackId& trackId) const override;
    muse::ValCh<bool> isTrackCollapsed(const trackedit::TrackId& trackId) const override;

    bool isSnapEnabled() const override;
    void setIsSnapEnabled(bool enabled) override;

    SnapType snapType() const override;
    void setSnapType(SnapType type) override;

    bool isSnapTripletsEnabled() const override;
    void setIsSnapTripletsEnabled(bool enabled) override;

    void setSnap(const Snap& s) override;
    Snap getSnap() const override;
    muse::ValCh<Snap> snap() const override;

    // State of user interaction
    double mousePositionY() const override;
    void setMousePositionY(double y) override;

    muse::ValCh<int> tracksVericalY() const override;
    void changeTracksVericalY(int deltaY) override;
    virtual muse::ValCh<bool> tracksVerticalScrollLocked() const override;
    virtual void setTracksVerticalScrollLocked(bool lock) override;

    int trackYPosition(const trackedit::TrackId& trackId) const override;
    void changeTrackHeight(const trackedit::TrackId& trackId, int deltaY) override;

    void setClipEditStartTimeOffset(double val) override;
    double clipEditStartTimeOffset() const override;

    void setClipEditEndTimeOffset(double val) override;
    double clipEditEndTimeOffset() const override;

    void setMoveInitiated(bool val) override;
    bool moveInitiated() const override;

    void setLastEditedClip(const trackedit::ClipKey& clipKey) override;
    trackedit::ClipKey lastEditedClip() const override;

    muse::ValCh<bool> altPressed() const override;
    muse::ValCh<bool> ctrlPressed() const override;

private:
    struct TrackData {
        muse::ValCh<int> height;
        muse::ValCh<bool> collapsed;
    };

    TrackData& makeTrackData(const trackedit::TrackId& trackId) const;

    bool eventFilter(QObject* watched, QEvent* event) override;

    muse::ValCh<int> m_tracksVericalY;
    muse::ValCh<bool> m_tracksVerticalScrollLocked;

    mutable std::map<trackedit::TrackId, TrackData> m_tracks;
    muse::ValCh<Snap> m_snap;

    muse::ValCh<double> m_mouseYPosition;

    //! Offset between mouse click position on clip's header and clip's start and end time
    double m_clipEditStartTimeOffset = -1.0;
    double m_clipEditEndTimeOffset = -1.0;

    //! User needs to drag a mouse by a certain amount of pixels (left or right) or
    //! move to the other track for move to be initiated
    bool m_moveInitiated = false;

    trackedit::ClipKey m_lastEditedClip = trackedit::ClipKey{};

    muse::ValCh<bool> m_altPressed;
    muse::ValCh<bool> m_ctrlPressed;
};
}
