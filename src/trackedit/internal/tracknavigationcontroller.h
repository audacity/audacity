/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "framework/actions/actionable.h"
#include "framework/global/async/asyncable.h"
#include "framework/rcommand/commandable.h"

#include "framework/global/modularity/ioc.h"
#include "framework/actions/iactionsdispatcher.h"
#include "framework/rcommand/icommanddispatcher.h"
#include "framework/ui/inavigationcontroller.h"
#include "playback/iplaybackcontroller.h"
#include "trackedit/iselectioncontroller.h"
#include "context/iglobalcontext.h"
#include "trackedit/itrackeditinteraction.h"

#include "trackedit/internal/itracknavigationcontroller.h"

namespace au::trackedit {
enum class SelectionDirection {
    Up,
    Down
};

class TrackNavigationController : public ITrackNavigationController, public muse::actions::Actionable, public muse::rcommand::Commandable,
    public muse::async::Asyncable, public muse::Contextable
{
    muse::ContextInject<muse::actions::IActionsDispatcher> dispatcher{ this };
    muse::ContextInject<muse::rcommand::ICommandDispatcher> commandDispatcher{ this };
    muse::ContextInject<muse::ui::INavigationController> navigationController{ this };
    muse::ContextInject<au::context::IGlobalContext> globalContext{ this };
    muse::ContextInject<au::playback::IPlaybackController> playbackController{ this };
    muse::ContextInject<au::trackedit::ISelectionController> selectionController{ this };
    muse::ContextInject<au::trackedit::ITrackeditInteraction> trackeditInteraction{ this };

public:
    TrackNavigationController(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    void init();

    bool isNavigationEnabled() const override;
    void setIsNavigationActive(bool active) override;
    muse::async::Notification isNavigationActiveChanged() const override;

    TrackId focusedTrack() const override;
    muse::async::Channel<TrackId, bool /*highlight*/> focusedTrackChanged() const override;

    TrackFocus focus() const override;
    void setFocus(const TrackFocus& focus, bool highlight = false) override;
    muse::async::Channel<TrackFocus, bool /*highlight*/> focusChanged() const override;

    void resetNavigation() override;

    muse::async::Channel<TrackItemKey> openContextMenuRequested() const override;
    muse::async::Channel<TrackId> openRulerContextMenuRequested() const override;

private:
    friend class TrackNavigationControllerTests;

    TrackItemKey focusedItemKey() const;
    bool isFocusedItemValid() const;
    bool isFocusedItemLabel() const;

    TrackItemKeyList sortedItemsKeys(const TrackId& trackId) const;

    bool isTrackItemsEmpty(const TrackId& trackId) const;
    bool isFirstTrack(const TrackId& trackId) const;
    bool isLastTrack(const TrackId& trackId) const;

    muse::Ret navigateToNextPanel();
    muse::Ret navigateToPrevPanel();

    bool navigateToAdjacentItem(bool next);

    void navigateToPrevTrack();
    void navigateToNextTrack();
    muse::Ret navigateToFirstTrack();
    muse::Ret navigateToLastTrack();

    muse::Ret navigateToAboveItem();
    muse::Ret navigateToBelowItem();
    void navigateToAdjacentRuler(SelectionDirection direction);
    void navigateToFirstItem();
    void navigateToLastItem();

    TrackItemKey findClosestItemOnTrack(const TrackId& trackId, double referenceStartTime) const;
    double itemStartTime(const TrackItemKey& key) const;

    muse::Ret replaceSelection();
    muse::Ret toggleSelection();
    muse::Ret rangeSelection();

    muse::Ret multiSelectionUp();
    muse::Ret multiSelectionDown();

    void updateSelectionStart(SelectionDirection direction);
    void updateTrackSelection(TrackIdList& selectedTracks, const TrackId& previousFocusedTrack);

    muse::Ret openContextMenuForFocusedItem();
    muse::Ret openContextMenuForFocusedRuler();

    void au3SetTrackFocused(const TrackId& trackId);

    void revalidateFocusedTrack();

    bool m_isNavigationActive = false;
    muse::async::Notification m_isNavigationActiveChannel;

    std::optional<TrackId> m_selectionStart;
    std::optional<TrackId> m_lastSelectedTrack;

    std::optional<double> m_savedItemStartTime;

    TrackFocus m_focus;
    muse::async::Channel<TrackFocus, bool /*highlight*/> m_focusChanged;
    muse::async::Channel<TrackId, bool /*highlight*/> m_focusedTrackChanged;

    muse::async::Channel<TrackItemKey> m_openContextMenuRequested;
    muse::async::Channel<TrackId> m_openRulerContextMenuRequested;
};
}
