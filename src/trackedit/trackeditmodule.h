/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/imodulesetup.h"

namespace au::trackedit {
class TrackeditActionsController;
class TrackeditUiActions;
class Au3SelectionController;
class TrackeditConfiguration;
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
    void onDeinit() override;

private:
    const std::shared_ptr<TrackeditActionsController> m_trackeditController;
    const std::shared_ptr<TrackeditUiActions> m_trackeditUiActions;
    const std::shared_ptr<Au3SelectionController> m_selectionController;
    const std::shared_ptr<TrackeditConfiguration> m_configuration;
    const std::shared_ptr<TrackNavigationController> m_trackNavigationController;
    const std::shared_ptr<TrackSpectrogramSettingsUpdater> m_trackSpectrogramSettingsUpdater;
};
}
