#ifndef AU_PROJECTSCENE_PROJECTSCENEACTIONCONTROLLER_H
#define AU_PROJECTSCENE_PROJECTSCENEACTIONCONTROLLER_H

#include "actions/actionable.h"
#include "modularity/ioc.h"
#include "actions/iactionsdispatcher.h"
#include "../iplaybackcontroller.h"

namespace au::projectscene {
class ProjectSceneActionController : public muse::actions::Actionable
{
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher;
    muse::Inject<IPlaybackController> playbackController;

public:
    ProjectSceneActionController() = default;

    void init();

private:

    void play();
    void stop();
    void rewind();

    void openAudioSetup();
};
}

#endif // AU_PROJECTSCENE_PROJECTSCENEACTIONCONTROLLER_H
