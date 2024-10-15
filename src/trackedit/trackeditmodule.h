/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/imodulesetup.h"

namespace au::trackedit {
class TrackeditActionsController;
class TrackeditUiActions;
class Au3SelectionController;
class TrackeditModule : public muse::modularity::IModuleSetup
{
public:

    std::string moduleName() const override;
    void registerExports() override;
    void resolveImports() override;
    void onInit(const muse::IApplication::RunMode& mode) override;
    void onDeinit() override;

private:
    std::shared_ptr<TrackeditActionsController> m_trackeditController;
    std::shared_ptr<TrackeditUiActions> m_trackeditUiActions;
    std::shared_ptr<Au3SelectionController> m_selectionController;
};
}
