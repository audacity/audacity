#include "projectconfiguration.h"

#include "global/settings.h"

using namespace au::project;

static const std::string module_name("project");

static const mu::Settings::Key USER_PROJECTS_PATH(module_name, "project/paths/myprojects");
static const mu::Settings::Key LAST_OPENED_PROJECTS_PATH(module_name, "project/paths/lastprojects");

mu::io::path_t ProjectConfiguration::userProjectsPath() const
{
    return mu::settings()->value(USER_PROJECTS_PATH).toPath();
}

void ProjectConfiguration::setUserProjectsPath(const mu::io::path_t& path)
{
    mu::settings()->setSharedValue(USER_PROJECTS_PATH, mu::Val(path));
    m_userProjectsPathChanged.send(path);
}

mu::async::Channel<mu::io::path_t> ProjectConfiguration::userProjectsPathChanged() const
{
    return m_userProjectsPathChanged;
}

mu::io::path_t ProjectConfiguration::defaultUserProjectsPath() const
{
    return mu::settings()->defaultValue(USER_PROJECTS_PATH).toPath();
}

mu::io::path_t ProjectConfiguration::lastOpenedProjectsPath() const
{
    return mu::settings()->value(LAST_OPENED_PROJECTS_PATH).toPath();
}

void ProjectConfiguration::setLastOpenedProjectsPath(const mu::io::path_t& path)
{
    mu::settings()->setSharedValue(LAST_OPENED_PROJECTS_PATH, mu::Val(path));
}
