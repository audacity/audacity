#ifndef AU_PROJECT_IPROJECTCONFIGURATION_H
#define AU_PROJECT_IPROJECTCONFIGURATION_H

#include "modularity/imoduleinterface.h"

#include "global/io/path.h"
#include "global/async/channel.h"
#include "async/notification.h"

#include "iaudacityproject.h"

namespace au::project {
class IProjectConfiguration : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IProjectConfiguration)

public:

    virtual ~IProjectConfiguration() = default;

    virtual muse::io::path_t recentFilesJsonPath() const = 0;
    virtual muse::ByteArray compatRecentFilesData() const = 0;

    virtual muse::io::path_t userProjectsPath() const = 0;
    virtual void setUserProjectsPath(const muse::io::path_t& path) = 0;
    virtual muse::async::Channel<muse::io::path_t> userProjectsPathChanged() const = 0;
    virtual muse::io::path_t defaultUserProjectsPath() const = 0;

    virtual muse::io::path_t lastOpenedProjectsPath() const = 0;
    virtual void setLastOpenedProjectsPath(const muse::io::path_t& path) = 0;

    virtual muse::io::path_t lastSavedProjectsPath() const = 0;
    virtual void setLastSavedProjectsPath(const muse::io::path_t& path) = 0;

    virtual muse::io::path_t defaultSavingFilePath(IAudacityProjectPtr project, const std::string& filenameAddition = "",
                                                   const std::string& suffix = "") const = 0;

    virtual SaveLocationType lastUsedSaveLocationType() const = 0;
    virtual void setLastUsedSaveLocationType(SaveLocationType type) = 0;

    virtual bool shouldAskSaveLocationType() const = 0;
    virtual void setShouldAskSaveLocationType(bool shouldAsk) = 0;

    virtual muse::io::path_t newProjectTemporaryPath() const = 0;

    virtual int homeProjectsPageTabIndex() const = 0;
    virtual void setHomeProjectsPageTabIndex(int index) = 0;

    enum class HomeProjectsPageViewType {
        Grid,
        List
    };

    virtual HomeProjectsPageViewType homeProjectsPageViewType() const = 0;
    virtual void setHomeProjectsPageViewType(HomeProjectsPageViewType type) = 0;

    virtual bool isAutoSaveEnabled() const = 0;
    virtual void setAutoSaveEnabled(bool enabled) = 0;
    virtual muse::async::Channel<bool> autoSaveEnabledChanged() const = 0;

    virtual int autoSaveIntervalMinutes() const = 0;
    virtual void setAutoSaveInterval(int minutes) = 0;
    virtual muse::async::Channel<int> autoSaveIntervalChanged() const = 0;
};
}

#endif // AU_PROJECT_IPROJECTCONFIGURATION_H
