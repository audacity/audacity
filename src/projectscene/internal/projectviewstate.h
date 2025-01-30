/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../iprojectviewstate.h"
#include "../iprojectsceneconfiguration.h"

#include "context/iglobalcontext.h"
#include "modularity/ioc.h"
#include "async/asyncable.h"

namespace au::projectscene {
class ProjectViewState : public IProjectViewState, public muse::async::Asyncable
{
    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<IProjectSceneConfiguration> configuration;

public:
    ProjectViewState();

    // context of all tracks
    muse::ValCh<int> tracksVericalY() const override;
    void changeTracksVericalY(int deltaY) override;

    double mousePositionY() const override;
    void setMousePositionY(double y) override;

    virtual muse::ValCh<bool> tracksVerticalScrollLocked() const override;
    virtual void setTracksVerticalScrollLocked(bool lock) override;

    // context of track
    int trackYPosition(const trackedit::TrackId& trackId) const override;
    muse::ValCh<int> trackHeight(const trackedit::TrackId& trackId) const override;
    muse::ValCh<bool> isTrackCollapsed(const trackedit::TrackId& trackId) const override;
    void changeTrackHeight(const trackedit::TrackId& trackId, int deltaY) override;

    bool isSnapEnabled() const override;
    void setIsSnapEnabled(bool enabled) override;

    SnapType snapType() const override;
    void setSnapType(SnapType type) override;

    bool isSnapTripletsEnabled() const override;
    void setIsSnapTripletsEnabled(bool enabled) override;

    void setSnap(const Snap& s) override;
    Snap getSnap() const override;
    muse::ValCh<Snap> snap() const override;

    void setClipEditStartTimeOffset(double val) override;
    double clipEditStartTimeOffset() override;

    void setClipEditEndTimeOffset(double val) override;
    double clipEditEndTimeOffset() override;

private:

    struct TrackData {
        muse::ValCh<int> height;
        muse::ValCh<bool> collapsed;
    };

    TrackData& makeTrackData(const trackedit::TrackId& trackId) const;

    muse::ValCh<int> m_tracksVericalY;
    muse::ValCh<bool> m_tracksVerticalScrollLocked;

    mutable std::map<trackedit::TrackId, TrackData> m_tracks;
    muse::ValCh<Snap> m_snap;

    muse::ValCh<double> m_mouseYPosition;

    //! Offset between mouse click position on clip's header and clip's start and end time
    double m_clipEditStartTimeOffset = -1.0;
    double m_clipEditEndTimeOffset = -1.0;
};
}
