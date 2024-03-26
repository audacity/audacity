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
class ProjectActionsController : public mu::actions::Actionable
{
    mu::Inject<mu::actions::IActionsDispatcher> dispatcher;
    mu::Inject<mu::context::IGlobalContext> globalContext;
    mu::Inject<mu::IInteractive> interactive;
    mu::Inject<IProjectConfiguration> configuration;
    mu::Inject<mu::io::IFileSystem> fileSystem;

public:
    ProjectActionsController() = default;

    void init();

    bool isProjectOpened(const mu::io::path_t& projectPath) const;

private:
    void newProject();
    void openProject(const mu::actions::ActionData& args);
    mu::Ret openProject(const mu::io::path_t& givenPath, const mu::String& displayNameOverride = mu::String());
    mu::Ret doOpenProject(const mu::io::path_t& filePath);
    mu::RetVal<IAudacityProjectPtr> loadProject(const mu::io::path_t& filePath);
    mu::io::path_t selectOpeningFile();

    mu::Ret openPageIfNeed(mu::Uri pageUri);

    bool m_isProjectProcessing = false;
};
}

#endif // AU_PROJECT_PROJECTACTIONSCONTROLLER_H
