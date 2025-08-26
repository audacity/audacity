/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "framework/actions/actionable.h"

#include "framework/global/modularity/ioc.h"
#include "trackedit/iselectioncontroller.h"

#include "trackedit/internal/itracknavigationcontroller.h"

namespace au::trackedit {
class TrackNavigationController : public ITrackNavigationController, public muse::actions::Actionable
{
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher;
    muse::Inject<au::trackedit::ISelectionController> selectionController;

public:
    void init();
    void navigateUp() override;
    void navigateDown() override;
    void toggleSelectionOnFocusedTrack() override;
};
}
