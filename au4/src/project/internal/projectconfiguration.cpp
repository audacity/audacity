#include "projectconfiguration.h"

#include "global/settings.h"

using namespace au::project;

static const std::string module_name("project");

static const muse::Settings::Key COMPAT_RECENT_FILES_DATA(module_name, "project/recentList");
static const muse::Settings::Key USER_PROJECTS_PATH(module_name, "project/paths/myprojects");
static const muse::Settings::Key LAST_OPENED_PROJECTS_PATH(module_name, "project/paths/lastprojects");
static const muse::Settings::Key HOME_PROJECTS_PAGE_VIEW_TYPE(module_name, "project/homeProjectsPageViewType");
static const muse::Settings::Key AUTOSAVE_ENABLED_KEY(module_name, "project/autoSaveEnabled");
static const muse::Settings::Key AUTOSAVE_INTERVAL_KEY(module_name, "project/autoSaveInterval");

static const std::string DEFAULT_FILE_SUFFIX(".aup4");

void ProjectConfiguration::init()
{
    muse::settings()->setDefaultValue(USER_PROJECTS_PATH, muse::Val(globalConfiguration()->userDataPath() + "/Projects"));
    muse::settings()->setDefaultValue(HOME_PROJECTS_PAGE_VIEW_TYPE, muse::Val(HomeProjectsPageViewType::Grid));
    muse::settings()->setDefaultValue(AUTOSAVE_ENABLED_KEY, muse::Val(true));
    muse::settings()->valueChanged(AUTOSAVE_ENABLED_KEY).onReceive(nullptr, [this](const muse::Val& val) {
        m_autoSaveEnabledChanged.send(val.toBool());
    });

    muse::settings()->setDefaultValue(AUTOSAVE_INTERVAL_KEY, muse::Val(5));
    muse::settings()->valueChanged(AUTOSAVE_INTERVAL_KEY).onReceive(nullptr, [this](const muse::Val& val) {
        m_autoSaveIntervalChanged.send(val.toInt());
    });
}

muse::io::path_t ProjectConfiguration::recentFilesJsonPath() const
{
    return globalConfiguration()->userAppDataPath().appendingComponent("recent_files.json");
}

muse::ByteArray ProjectConfiguration::compatRecentFilesData() const
{
    std::string data = muse::settings()->value(COMPAT_RECENT_FILES_DATA).toString();

    return muse::ByteArray(data.data(), data.size());
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

muse::io::path_t ProjectConfiguration::newProjectTemporaryPath() const
{
    return globalConfiguration()->userAppDataPath() + "/new_project" + DEFAULT_FILE_SUFFIX;
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

bool ProjectConfiguration::isAutoSaveEnabled() const
{
    return muse::settings()->value(AUTOSAVE_ENABLED_KEY).toBool();
}

void ProjectConfiguration::setAutoSaveEnabled(bool enabled)
{
    muse::settings()->setSharedValue(AUTOSAVE_ENABLED_KEY, muse::Val(enabled));
}

muse::async::Channel<bool> ProjectConfiguration::autoSaveEnabledChanged() const
{
    return m_autoSaveEnabledChanged;
}

int ProjectConfiguration::autoSaveIntervalMinutes() const
{
    return muse::settings()->value(AUTOSAVE_INTERVAL_KEY).toInt();
}

void ProjectConfiguration::setAutoSaveInterval(int minutes)
{
    muse::settings()->setSharedValue(AUTOSAVE_INTERVAL_KEY, muse::Val(minutes));
}

muse::async::Channel<int> ProjectConfiguration::autoSaveIntervalChanged() const
{
    return m_autoSaveIntervalChanged;
}
