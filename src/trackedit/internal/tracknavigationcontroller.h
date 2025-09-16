/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "framework/actions/actionable.h"
#include "async/asyncable.h"

#include "framework/global/modularity/ioc.h"
#include "trackedit/iselectioncontroller.h"
#include "actions/iactionsdispatcher.h"
#include "ui/inavigationcontroller.h"

#include "trackedit/internal/itracknavigationcontroller.h"

namespace au::trackedit {
enum class SelectionDirection {
    Up,
    Down
};

class TrackNavigationController : public ITrackNavigationController, public muse::actions::Actionable, public muse::async::Asyncable
{
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher;
    muse::Inject<au::trackedit::ISelectionController> selectionController;
    muse::Inject<muse::ui::INavigationController> navigationController;

public:
    void init();
    void focusTrackByIndex(const muse::actions::ActionData& args) override;
    void focusPrevTrack() override;
    void focusNextTrack() override;
    void navigateUp(const muse::actions::ActionData& args) override;
    void navigateDown(const muse::actions::ActionData& args) override;
    void toggleSelectionOnFocusedTrack() override;
    void multiSelectionUp() override;
    void multiSelectionDown() override;

private:
    void updateSelectionStart(SelectionDirection direction);
    void updateTrackSelection(TrackIdList& selectedTracks, const TrackId& previousFocusedTrack);

    std::optional<TrackId> m_selectionStart;
};
}
