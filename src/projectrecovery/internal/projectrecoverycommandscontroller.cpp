/*
 * Audacity: A Digital Audio Editor
 */
#include "projectrecoverycommandscontroller.h"

#include "../projectrecoverycommands.h"

using namespace au::projectrecovery;

void ProjectRecoveryCommandsController::init()
{
    commandDispatcher()->onRequest(this, PROJECT_RECOVERY_COMMAND, [this]() { return recoverProject(); });
}

void ProjectRecoveryCommandsController::deinit()
{
    commandDispatcher()->unreg(this);
}

muse::Ret ProjectRecoveryCommandsController::recoverProject()
{
    interactive()->open(PROJECT_RECOVERY_URI);
    return muse::make_ok();
}
