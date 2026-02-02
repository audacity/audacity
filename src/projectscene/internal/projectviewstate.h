/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "playback/iplaybackconfiguration.h"
#include "trackedit/iselectioncontroller.h"
#include "trackedit/iprojecthistory.h"
#include "../iprojectsceneconfiguration.h"

#include "au3wrap/iau3project.h"

#include "../iprojectviewstate.h"

#include <unordered_set>

namespace au::projectscene {
class ProjectViewState : public QObject, public IProjectViewState, public muse::Injectable, public muse::async::Asyncable
{
    Q_OBJECT

    muse::GlobalInject<IProjectSceneConfiguration> configuration;
    muse::GlobalInject<au::playback::IPlaybackConfiguration> playbackConfiguration;

    muse::Inject<au::context::IGlobalContext> globalContext{ this };
    muse::Inject<trackedit::ISelectionController> selectionController{ this };
    muse::Inject<trackedit::IProjectHistory> projectHistory{ this };

public:
    ProjectViewState(const muse::modularity::ContextPtr& ctx);

    void init(const std::shared_ptr<au3::IAu3Project>& project);

    // State of elements
    muse::ValCh<int> totalTrackHeight() const override;
    muse::ValCh<int> trackHeight(const trackedit::TrackId& trackId) const override;
    muse::ValCh<bool> isTrackCollapsed(const trackedit::TrackId& trackId) const override;
    muse::ValCh<double> channelHeightRatio(const trackedit::TrackId& trackId) const override;

    int trackVerticalPosition(const trackedit::TrackId& trackId) const override;
    void changeTrackHeight(const trackedit::TrackId& trackId, int delta) override;
    void setTrackHeight(const trackedit::TrackId& trackId, int height) override;
    void setChannelHeightRatio(const trackedit::TrackId& trackId, double ratio) override;
    trackedit::TrackId trackAtPosition(double y) const override;
    trackedit::TrackIdList tracksInRange(double y1, double y2) const override;

    bool isSnapEnabled() const override;
    void setIsSnapEnabled(bool enabled) override;

    SnapType snapType() const override;
    void setSnapType(SnapType type) override;

    bool isSnapTripletsEnabled() const override;
    void setIsSnapTripletsEnabled(bool enabled) override;

    void setSnap(const Snap& s) override;
    Snap getSnap() const override;
    muse::ValCh<Snap> snap() const override;

    void setAutomationEnabled(bool enabled) override;
    muse::ValCh<bool> automationEnabled() const override;

    void setSplitToolEnabled(bool enabled) override;
    muse::ValCh<bool> splitToolEnabled() const override;

    muse::ValCh<std::pair<float, float> > verticalDisplayBounds(const trackedit::TrackId& trackId) const override;
    void zoomInVertically(const trackedit::TrackId& trackId) override;
    void zoomOutVertically(const trackedit::TrackId& trackId) override;
    void resetVerticalZoom(const trackedit::TrackId& trackId) override;
    bool isDefaultVerticalZoom(const trackedit::TrackId& trackId) const override;
    bool isMaxVerticalZoom(const trackedit::TrackId& trackId) const override;
    bool isMinVerticalZoom(const trackedit::TrackId& trackId) const override;

    muse::ValCh<bool> isHalfWave(const trackedit::TrackId& trackId) const override;
    void toggleHalfWave(const trackedit::TrackId& trackId) override;

    muse::ValCh<trackedit::TrackViewType> trackViewType(const trackedit::TrackId& trackId) const override;
    void setTrackViewType(const trackedit::TrackId& trackId, trackedit::TrackViewType viewType) override;
    void toggleGlobalSpectrogramView() override;
    bool globalSpectrogramViewIsOn() const override;
    bool globalSpectrogramViewToggleIsActive() const override;
    muse::async::Notification globalSpectrogramViewToggleChanged() const override;

    muse::ValCh<int> trackRulerType(const trackedit::TrackId& trackId) const override;
    void setTrackRulerType(const trackedit::TrackId& trackId, int rulerType) override;

    muse::ValCh<int> verticalRulerWidth() const override;

    // State of user interaction
    double mousePositionY() const override;
    void setMousePositionY(double y) override;

    muse::ValCh<int> tracksVerticalOffset() const override;
    void changeTracksVerticalOffset(int deltaY) override;
    virtual muse::ValCh<bool> tracksVerticalScrollLocked() const override;
    virtual void setTracksVerticalScrollLocked(bool lock) override;

    void setItemEditStartTimeOffset(double val) override;
    double itemEditStartTimeOffset() const override;

    void setItemEditEndTimeOffset(double val) override;
    double itemEditEndTimeOffset() const override;

    void setMoveInitiated(bool val) override;
    bool moveInitiated() const override;

    void setLastEditedClip(const trackedit::ClipKey& clipKey) override;
    trackedit::ClipKey lastEditedClip() const override;

    void setItemsBoundaries(const std::set<muse::secs_t>& boundaries) override;
    std::set<muse::secs_t> itemsBoundaries() const override;
    void updateItemsBoundaries(bool excludeCurrentSelection,
                               const trackedit::TrackItemKey& itemKeyToOmit = trackedit::TrackItemKey {}) override;

    void setZoomState(const ZoomState& state) override;
    ZoomState zoomState() const override;

    muse::async::Notification rolledBack() const override;

    muse::ValCh<bool> altPressed() const override;
    muse::ValCh<bool> ctrlPressed() const override;

    int trackDefaultHeight() const override;

private:
    friend class ProjectViewStateTests;

    int calculateVerticalRulerWidth() const;
    float maxVerticalZoomLevel(const trackedit::TrackId& trackId) const;

    struct TrackData {
        muse::ValCh<int> height;
        muse::ValCh<bool> collapsed;
        muse::ValCh<double> channelHeightRatio;
        muse::ValCh<std::pair<float, float> > verticalDisplayBounds;
        muse::ValCh<bool> isHalfWave;
        muse::ValCh<int> rulerType;
        muse::ValCh<trackedit::TrackViewType> viewType;
    };

    mutable muse::ValCh<int> m_totalTracksHeight;
    mutable muse::ValCh<int> m_verticalRulerWidth;

    TrackData& makeTrackData(const trackedit::TrackId& trackId) const;

    bool eventFilter(QObject* watched, QEvent* event) override;

    muse::ValCh<int> m_tracksVerticalOffset;
    muse::ValCh<bool> m_tracksVerticalScrollLocked;

    mutable std::map<trackedit::TrackId, TrackData> m_tracks;
    muse::ValCh<Snap> m_snap;

    muse::ValCh<bool> m_automationEnabled;

    muse::ValCh<bool> m_splitToolEnabled;

    muse::ValCh<std::unordered_set<trackedit::TrackId> > m_spectrogramToggledTracks;
    muse::async::Notification m_globalSpectrogramViewToggleChanged;

    muse::ValCh<double> m_mouseYPosition;

    //! Offset between mouse click position on item's header and item's start and end time
    double m_itemEditStartTimeOffset = -1.0;
    double m_itemEditEndTimeOffset = -1.0;

    //! User needs to drag a mouse by a certain amount of pixels (left or right) or
    //! move to the other track for move to be initiated
    bool m_moveInitiated = false;

    trackedit::ClipKey m_lastEditedClip = trackedit::ClipKey{};

    //! start/end times the currently moved/trimmed/stretched item can snap to
    std::set<muse::secs_t> m_itemsBoundaries;

    muse::ValCh<bool> m_altPressed;
    muse::ValCh<bool> m_ctrlPressed;

    muse::async::Notification m_rolledBack;
};
}
