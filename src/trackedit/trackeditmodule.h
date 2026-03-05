/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/imodulesetup.h"

namespace au::trackedit {
class TrackeditConfiguration;
class TrackeditActionsController;
class TrackeditUiActions;
class Au3SelectionController;
class TrackNavigationController;
class TrackSpectrogramSettingsUpdater;

class TrackeditModule : public muse::modularity::IModuleSetup
{
public:
    TrackeditModule();

    std::string moduleName() const override;
    void registerExports() override;
    void registerResources() override;
    void registerUiTypes() override;
    void resolveImports() override;
    void onInit(const muse::IApplication::RunMode& mode) override;

    muse::modularity::IContextSetup* newContext(const muse::modularity::ContextPtr& ctx) const override;

private:
    std::shared_ptr<TrackeditConfiguration> m_configuration;
};

class TrackeditContext : public muse::modularity::IContextSetup
{
public:
    TrackeditContext(const muse::modularity::ContextPtr& ctx)
        : muse::modularity::IContextSetup(ctx) {}

    void registerExports() override;
    void onInit(const muse::IApplication::RunMode& mode) override;
    void onDeinit() override;

private:
    std::shared_ptr<TrackeditActionsController> m_trackeditController;
    std::shared_ptr<TrackeditUiActions> m_trackeditUiActions;
    std::shared_ptr<Au3SelectionController> m_selectionController;
    std::shared_ptr<TrackNavigationController> m_trackNavigationController;
    std::shared_ptr<TrackSpectrogramSettingsUpdater> m_trackSpectrogramSettingsUpdater;
};
}
