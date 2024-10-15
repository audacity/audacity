#ifndef AU_PROJECT_PROJECTCONFIGURATION_H
#define AU_PROJECT_PROJECTCONFIGURATION_H

#include "types/val.h"

#include "modularity/ioc.h"
#include "global/iglobalconfiguration.h"

#include "../iprojectconfiguration.h"

namespace au::project {
class ProjectConfiguration : public IProjectConfiguration
{
    muse::Inject<muse::IGlobalConfiguration> globalConfiguration;

public:
    ProjectConfiguration() = default;

    void init();

    muse::io::path_t recentFilesJsonPath() const override;
    muse::ByteArray compatRecentFilesData() const override;

    muse::io::path_t userProjectsPath() const override;
    void setUserProjectsPath(const muse::io::path_t& path) override;
    muse::async::Channel<muse::io::path_t> userProjectsPathChanged() const override;
    muse::io::path_t defaultUserProjectsPath() const override;

    muse::io::path_t lastOpenedProjectsPath() const override;
    void setLastOpenedProjectsPath(const muse::io::path_t& path) override;

    muse::io::path_t lastSavedProjectsPath() const override;
    void setLastSavedProjectsPath(const muse::io::path_t& path) override;

    muse::io::path_t defaultSavingFilePath(IAudacityProjectPtr project, const std::string& filenameAddition = "",
                                           const std::string& suffix = "") const override;

    SaveLocationType lastUsedSaveLocationType() const override;
    void setLastUsedSaveLocationType(SaveLocationType type) override;

    bool shouldAskSaveLocationType() const override;
    void setShouldAskSaveLocationType(bool shouldAsk) override;

    muse::io::path_t newProjectTemporaryPath() const override;

    int homeProjectsPageTabIndex() const override;
    void setHomeProjectsPageTabIndex(int index) override;

    HomeProjectsPageViewType homeProjectsPageViewType() const override;
    void setHomeProjectsPageViewType(HomeProjectsPageViewType type) override;

    bool isAutoSaveEnabled() const override;
    void setAutoSaveEnabled(bool enabled) override;
    muse::async::Channel<bool> autoSaveEnabledChanged() const override;

    int autoSaveIntervalMinutes() const override;
    void setAutoSaveInterval(int minutes) override;
    muse::async::Channel<int> autoSaveIntervalChanged() const override;

private:
    muse::async::Channel<muse::io::path_t> m_userProjectsPathChanged;

    int m_homeProjectsPageTabIndex = 0;
    muse::async::Notification m_homeProjectsPageTabIndexChanged;

    muse::async::Channel<bool> m_autoSaveEnabledChanged;
    muse::async::Channel<int> m_autoSaveIntervalChanged;
};
}

#endif // AU_PROJECT_PROJECTCONFIGURATION_H
