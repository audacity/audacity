#include "projectconfiguration.h"

#include "global/settings.h"

using namespace au::project;

static const std::string module_name("project");

static const muse::Settings::Key USER_PROJECTS_PATH(module_name, "project/paths/myprojects");
static const muse::Settings::Key LAST_OPENED_PROJECTS_PATH(module_name, "project/paths/lastprojects");
static const muse::Settings::Key HOME_PROJECTS_PAGE_VIEW_TYPE(module_name, "project/homeProjectsPageViewType");

void ProjectConfiguration::init()
{
    muse::settings()->setDefaultValue(HOME_PROJECTS_PAGE_VIEW_TYPE, muse::Val(HomeProjectsPageViewType::Grid));
}

muse::io::path_t ProjectConfiguration::userProjectsPath() const
{
    return muse::settings()->value(USER_PROJECTS_PATH).toPath();
}

void ProjectConfiguration::setUserProjectsPath(const muse::io::path_t& path)
{
    muse::settings()->setSharedValue(USER_PROJECTS_PATH, muse::Val(path));
    m_userProjectsPathChanged.send(path);
}

muse::async::Channel<muse::io::path_t> ProjectConfiguration::userProjectsPathChanged() const
{
    return m_userProjectsPathChanged;
}

muse::io::path_t ProjectConfiguration::defaultUserProjectsPath() const
{
    return muse::settings()->defaultValue(USER_PROJECTS_PATH).toPath();
}

muse::io::path_t ProjectConfiguration::lastOpenedProjectsPath() const
{
    return muse::settings()->value(LAST_OPENED_PROJECTS_PATH).toPath();
}

void ProjectConfiguration::setLastOpenedProjectsPath(const muse::io::path_t& path)
{
    muse::settings()->setSharedValue(LAST_OPENED_PROJECTS_PATH, muse::Val(path));
}

int ProjectConfiguration::homeProjectsPageTabIndex() const
{
    return m_homeProjectsPageTabIndex;
}

void ProjectConfiguration::setHomeProjectsPageTabIndex(int index)
{
    m_homeProjectsPageTabIndex = index;
}

IProjectConfiguration::HomeProjectsPageViewType ProjectConfiguration::homeProjectsPageViewType() const
{
    return muse::settings()->value(HOME_PROJECTS_PAGE_VIEW_TYPE).toEnum<HomeProjectsPageViewType>();
}

void ProjectConfiguration::setHomeProjectsPageViewType(HomeProjectsPageViewType type)
{
    muse::settings()->setLocalValue(HOME_PROJECTS_PAGE_VIEW_TYPE, muse::Val(type));
}
