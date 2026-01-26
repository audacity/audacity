/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "framework/actions/actionable.h"
#include "framework/global/async/asyncable.h"

#include "framework/global/modularity/ioc.h"
#include "framework/actions/iactionsdispatcher.h"
#include "framework/ui/inavigationcontroller.h"
#include "trackedit/iselectioncontroller.h"
#include "context/iglobalcontext.h"
#include "itrackeditinteraction.h"

#include "trackedit/internal/itracknavigationcontroller.h"

namespace au::trackedit {
enum class SelectionDirection {
    Up,
    Down
};

class TrackNavigationController : public ITrackNavigationController, public muse::actions::Actionable, public muse::async::Asyncable,
    public muse::Injectable
{
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher{ this };
    muse::Inject<muse::ui::INavigationController> navigationController{ this };
    muse::Inject<au::context::IGlobalContext> globalContext{ this };
    muse::Inject<au::trackedit::ISelectionController> selectionController{ this };
    muse::Inject<au::trackedit::ITrackeditInteraction> trackeditInteraction{ this };

public:
    TrackNavigationController(const muse::modularity::ContextPtr& ctx)
        : muse::Injectable(ctx) {}

    void init();

    TrackId focusedTrack() const override;
    void setFocusedTrack(const TrackId &trackId) override;
    muse::async::Notification focusedTrackChanged() const override;

    TrackItemKey focusedItem() const override;
    void setFocusedItem(const TrackItemKey& key) override;
    muse::async::Notification focusedItemChanged() const override;

    void toggleSelectionOnFocusedTrack() override;
    void trackRangeSelection() override;
    void multiSelectionUp() override;
    void multiSelectionDown() override;

    muse::async::Channel<TrackItemKey> openContextMenuRequested() const override;

    void navigateNextItem();
    void navigatePrevItem();

    void navigateLastItem();

    void moveFocusedItemLeft();
    void moveFocusedItemRight();
    void extendFocusedItemBoundaryLeft();
    void extendFocusedItemBoundaryRight();
    void reduceFocusedItemBoundaryLeft();
    void reduceFocusedItemBoundaryRight();
    void moveFocusedItemUp();
    void moveFocusedItemDown();

    void openContextMenuForFocusedItem();

private:
    void updateSelectionStart(SelectionDirection direction);
    void updateTrackSelection(TrackIdList& selectedTracks, const TrackId& previousFocusedTrack);

    double zoomLevel() const;
    double calculateStepSize() const;

    void navigateNextPanel();
    void navigatePrevPanel();

    void focusTrackByIndex(const muse::actions::ActionData& args);
    void focusPrevTrack();
    void focusNextTrack();
    void focusFirstTrack();
    void focusLastTrack();

    TrackItemKey focusedItemKey() const;
    bool isFocusedItemValid() const;
    bool isFocusedItemLabel() const;

    TrackItemKeyList sortedItemsKeys(const TrackId& trackId) const;
    Label focusedLabel() const;

    bool isTrackItemsEmpty(const TrackId& trackId) const;

    TrackId resolvePreviousTrackIdForMove(const TrackId& trackId) const;
    TrackId resolveNextTrackIdForMove(const TrackId& trackId) const;

    std::optional<TrackId> m_selectionStart;
    std::optional<TrackId> m_lastSelectedTrack;

    TrackItemKey m_focusedItemKey;
    muse::async::Notification m_focusedItemChanged;
    muse::async::Notification m_focusedTrackChanged;

    muse::async::Channel<TrackItemKey> m_openContextMenuRequested;
};
}
