/*
* Audacity: A Digital Audio Editor
*/

#include "projectsceneactionscontroller.h"

using namespace muse;
using namespace au::projectscene;
using namespace muse::async;
using namespace muse::actions;

void ProjectSceneActionsController::init()
{
    dispatcher()->reg(this, "minutes-seconds-ruler", this, &ProjectSceneActionsController::toggleTimelineRuler);
    dispatcher()->reg(this, "beats-measures-ruler", this, &ProjectSceneActionsController::toggleBeatsRuler);
    dispatcher()->reg(this, "toggle-vertical-rulers", this, &ProjectSceneActionsController::toggleVerticalRulers);
    dispatcher()->reg(this, "update-display-while-playing", this, &ProjectSceneActionsController::updateDisplayWhilePlaying);
    dispatcher()->reg(this, "pinned-play-head", this, &ProjectSceneActionsController::pinnedPlayHead);
}

void ProjectSceneActionsController::notifyActionCheckedChanged(const ActionCode& actionCode)
{
    m_actionCheckedChanged.send(actionCode);
}

void ProjectSceneActionsController::toggleTimelineRuler()
{
    NOT_IMPLEMENTED;
}

void ProjectSceneActionsController::toggleBeatsRuler()
{
    NOT_IMPLEMENTED;
}

void ProjectSceneActionsController::toggleVerticalRulers()
{
    NOT_IMPLEMENTED;
}

void ProjectSceneActionsController::updateDisplayWhilePlaying()
{
    NOT_IMPLEMENTED;
}

void ProjectSceneActionsController::pinnedPlayHead()
{
    NOT_IMPLEMENTED;
}

bool ProjectSceneActionsController::actionChecked(const ActionCode& actionCode) const
{
    //! TODO AU4
    return false;
}

Channel<ActionCode> ProjectSceneActionsController::actionCheckedChanged() const
{
    return m_actionCheckedChanged;
}

bool ProjectSceneActionsController::canReceiveAction(const ActionCode&) const
{
    return globalContext()->currentProject() != nullptr;
}
