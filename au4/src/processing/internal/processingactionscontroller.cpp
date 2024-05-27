/*
* Audacity: A Digital Audio Editor
*/
#include "processingactionscontroller.h"

using namespace muse;
using namespace au::processing;
using namespace muse::async;
using namespace muse::actions;

void ProcessingActionsController::init()
{
    dispatcher()->reg(this, "toggle-loop-region", this, &ProcessingActionsController::toggleLoopRegion);
    dispatcher()->reg(this, "clear-loop-region", this, &ProcessingActionsController::clearLoopRegion);
    dispatcher()->reg(this, "set-loop-region-to-selection", this, &ProcessingActionsController::setLoopRegionToSelection);
    dispatcher()->reg(this, "set-selection-to-loop", this, &ProcessingActionsController::setSelectionToLoop);
    dispatcher()->reg(this, "set-loop-region-in", this, &ProcessingActionsController::setLoopRegionIn);
    dispatcher()->reg(this, "set-loop-region-out", this, &ProcessingActionsController::setLoopRegionOut);
}

void ProcessingActionsController::notifyActionCheckedChanged(const ActionCode& actionCode)
{
    m_actionCheckedChanged.send(actionCode);
}

void ProcessingActionsController::toggleLoopRegion()
{
    NOT_IMPLEMENTED;
}

void ProcessingActionsController::clearLoopRegion()
{
    NOT_IMPLEMENTED;
}

void ProcessingActionsController::setLoopRegionToSelection()
{
    NOT_IMPLEMENTED;
}

void ProcessingActionsController::setSelectionToLoop()
{
    NOT_IMPLEMENTED;
}

void ProcessingActionsController::setLoopRegionIn()
{
    NOT_IMPLEMENTED;
}

void ProcessingActionsController::setLoopRegionOut()
{
    NOT_IMPLEMENTED;
}

bool ProcessingActionsController::actionChecked(const ActionCode& actionCode) const
{
    //! TODO AU4
    return false;
}

Channel<ActionCode> ProcessingActionsController::actionCheckedChanged() const
{
    return m_actionCheckedChanged;
}

bool ProcessingActionsController::canReceiveAction(const ActionCode&) const
{
    return globalContext()->currentProject() != nullptr;
}
