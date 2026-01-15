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

class TrackNavigationController : public ITrackNavigationController, public muse::actions::Actionable, public muse::async::Asyncable
{
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher;
    muse::Inject<muse::ui::INavigationController> navigationController;
    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<au::trackedit::ISelectionController> selectionController;
    muse::Inject<au::trackedit::ITrackeditInteraction> trackeditInteraction;

public:
    void init();

    void focusTrackByIndex(const muse::actions::ActionData& args) override;
    void focusPrevTrack() override;
    void focusNextTrack() override;

    void setFocusedItem(const TrackItemKey& key) override;
    muse::async::Channel<TrackItemKey> focusedItemChanged() const override;

    void navigateUp(const muse::actions::ActionData& args) override;
    void navigateDown(const muse::actions::ActionData& args) override;
    void toggleSelectionOnFocusedTrack() override;
    void trackRangeSelection() override;
    void multiSelectionUp() override;
    void multiSelectionDown() override;

    void moveFocusedItemLeft();
    void moveFocusedItemRight();
    void extendFocusedItemBoundaryLeft();
    void extendFocusedItemBoundaryRight();
    void reduceFocusedItemBoundaryLeft();
    void reduceFocusedItemBoundaryRight();
    void moveFocusedItemUp();
    void moveFocusedItemDown();

private:
    void updateSelectionStart(SelectionDirection direction);
    void updateTrackSelection(TrackIdList& selectedTracks, const TrackId& previousFocusedTrack);

    double zoomLevel() const;
    double calculateStepSize() const;

    TrackItemKey focusedItemKey() const;
    bool isFocusedItemValid() const;
    bool isFocusedItemLabel() const;

    Label focusedLabel() const;

    TrackId resolvePreviousTrackId(const TrackId& trackId) const;
    TrackId resolveNextTrackId(const TrackId& trackId) const;

    std::optional<TrackId> m_selectionStart;
    std::optional<TrackId> m_lastSelectedTrack;

    TrackItemKey m_focusedItemKey;
    muse::async::Channel<TrackItemKey> m_focusedItemChanged;
};
}
