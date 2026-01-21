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
    void setFocusedTrack(TrackId trackId) override;
    muse::async::Channel<TrackId> focusedTrackChanged() const override;

    void focusTrackByIndex(const muse::actions::ActionData& args) override;
    void focusPrevTrack() override;
    void focusNextTrack() override;

    void setFocusedItem(const TrackItemKey& key) override;
    muse::async::Channel<TrackItemKey> focusedItemChanged() const override;

    void toggleSelectionOnFocusedTrack() override;
    void trackRangeSelection() override;
    void multiSelectionUp() override;
    void multiSelectionDown() override;

    muse::async::Channel<TrackItemKey> openContextMenuRequested() const override;

    void navigateNextItem();
    void navigatePrevItem();

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

    TrackItemKey focusedItemKey() const;
    bool isFocusedItemValid() const;
    bool isFocusedItemLabel() const;

    Label focusedLabel() const;

    TrackId resolvePreviousTrackIdForMove(const TrackId& trackId) const;
    TrackId resolveNextTrackIdForMove(const TrackId& trackId) const;

    template<typename T>
    struct Val {
        Val()
            : val(T()) {}
        Val(const T& val)
            : val(val) {}
        T val;
        muse::async::Channel<T> changed;
        muse::async::Channel<T> selected;

        void set(const T& v, bool complete)
        {
            if (val == v) {
                return;
            }
            val = v;
            changed.send(v);
            if (complete) {
                selected.send(v);
            }
        }
    };

    std::optional<TrackId> m_selectionStart;
    std::optional<TrackId> m_lastSelectedTrack;

    TrackItemKey m_focusedItemKey;
    muse::async::Channel<TrackItemKey> m_focusedItemChanged;

    muse::async::Channel<TrackItemKey> m_openContextMenuRequested;

    Val<TrackId> m_focusedTrack = Val<TrackId> { TrackId(INVALID_TRACK) };
};
}
