#ifndef MU_PROJECTSCENE_PROJECTSCENEACTIONCONTROLLER_H
#define MU_PROJECTSCENE_PROJECTSCENEACTIONCONTROLLER_H

#include "modularity/ioc.h"
#include "actions/iactionsdispatcher.h"
#include "actions/actionable.h"

namespace au::projectscene {
class ProjectSceneActionController : public mu::actions::Actionable
{
    mu::Inject<mu::actions::IActionsDispatcher> dispatcher;

public:
    ProjectSceneActionController() = default;

    void init();

private:

    void play();
    void stop();
    void rewind();
};
}

#endif // MU_PROJECTSCENE_PROJECTSCENEACTIONCONTROLLER_H
