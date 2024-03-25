#include "projectsceneactioncontroller.h"

#include "log.h"

using namespace au::projectscene;

void ProjectSceneActionController::init()
{
    dispatcher()->reg(this, "play", this, &ProjectSceneActionController::play);
    dispatcher()->reg(this, "stop", this, &ProjectSceneActionController::stop);
    dispatcher()->reg(this, "rewind", this, &ProjectSceneActionController::rewind);
}

void ProjectSceneActionController::play()
{
    NOT_IMPLEMENTED;
}

void ProjectSceneActionController::stop()
{
    NOT_IMPLEMENTED;
}

void ProjectSceneActionController::rewind()
{
    NOT_IMPLEMENTED;
}
