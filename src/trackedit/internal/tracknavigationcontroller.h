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
    void setFocusedTrack(const TrackId& trackId, bool highlight = false) override;
    muse::async::Channel<TrackId, bool /*highlight*/> focusedTrackChanged() const override;

    TrackItemKey focusedItem() const override;
    void setFocusedItem(const TrackItemKey& key, bool highlight = false) override;
    muse::async::Channel<TrackItemKey, bool /*highlight*/> focusedItemChanged() const override;

    muse::async::Channel<TrackItemKey> openContextMenuRequested() const override;

private:
    double zoomLevel() const;
    double calculateStepSize() const;

    TrackItemKey focusedItemKey() const;
    bool isFocusedItemValid() const;
    bool isFocusedItemLabel() const;

    TrackItemKeyList sortedItemsKeys(const TrackId& trackId) const;
    Label focusedLabel() const;

    bool isTrackItemsEmpty(const TrackId& trackId) const;

    void navigateToNextPanel();
    void navigateToPrevPanel();

    void navigateToPrevTrack();
    void navigateToNextTrack();
    void navigateToFirstTrack();
    void navigateToLastTrack();

    void navigateToNextItem();
    void navigateToPrevItem();
    void navigateToFirstItem();
    void navigateToLastItem();

    void moveFocusedItemLeft();
    void moveFocusedItemRight();
    void moveFocusedItemUp();
    void moveFocusedItemDown();
    void extendFocusedItemBoundaryLeft();
    void extendFocusedItemBoundaryRight();
    void reduceFocusedItemBoundaryLeft();
    void reduceFocusedItemBoundaryRight();

    TrackId resolvePreviousTrackIdForMove(const TrackId& trackId) const;
    TrackId resolveNextTrackIdForMove(const TrackId& trackId) const;

    void toggleSelection();
    void trackRangeSelection();

    void multiSelectionUp();
    void multiSelectionDown();

    void updateSelectionStart(SelectionDirection direction);
    void updateTrackSelection(TrackIdList& selectedTracks, const TrackId& previousFocusedTrack);

    void openContextMenuForFocusedItem();

    std::optional<TrackId> m_selectionStart;
    std::optional<TrackId> m_lastSelectedTrack;

    TrackItemKey m_focusedItemKey;
    muse::async::Channel<TrackItemKey, bool /*highlight*/> m_focusedItemChanged;
    muse::async::Channel<TrackId, bool /*highlight*/> m_focusedTrackChanged;

    muse::async::Channel<TrackItemKey> m_openContextMenuRequested;
};
}
