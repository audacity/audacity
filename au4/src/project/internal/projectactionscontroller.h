#ifndef AU_PROJECT_PROJECTACTIONSCONTROLLER_H
#define AU_PROJECT_PROJECTACTIONSCONTROLLER_H

#include "actions/actionable.h"
#include "modularity/ioc.h"
#include "actions/iactionsdispatcher.h"
#include "context/iglobalcontext.h"
#include "global/iinteractive.h"
#include "global/io/ifilesystem.h"
#include "../iprojectconfiguration.h"

#include "../iaudacityproject.h"

namespace au::project {
class ProjectActionsController : public muse::actions::Actionable
{
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher;
    muse::Inject<mu::context::IGlobalContext> globalContext;
    muse::Inject<muse::IInteractive> interactive;
    muse::Inject<IProjectConfiguration> configuration;
    muse::Inject<muse::io::IFileSystem> fileSystem;

public:
    ProjectActionsController() = default;

    void init();

    bool isProjectOpened(const muse::io::path_t& projectPath) const;

private:
    void newProject();
    void openProject(const muse::actions::ActionData& args);
    muse::Ret openProject(const muse::io::path_t& givenPath, const muse::String& displayNameOverride = muse::String());
    muse::Ret doOpenProject(const muse::io::path_t& filePath);
    muse::RetVal<IAudacityProjectPtr> loadProject(const muse::io::path_t& filePath);
    muse::io::path_t selectOpeningFile();

    muse::Ret openPageIfNeed(muse::Uri pageUri);

    bool m_isProjectProcessing = false;
};
}

#endif // AU_PROJECT_PROJECTACTIONSCONTROLLER_H
