/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_PROСESSING_PROСESSINGMODULE_H
#define AU_PROСESSING_PROСESSINGMODULE_H

#include "modularity/imodulesetup.h"

namespace au::processing {
class ProcessingController;
class ProcessingUiActions;
class ProcessingModule : public muse::modularity::IModuleSetup
{
public:

    std::string moduleName() const override;
    void registerExports() override;
    void resolveImports() override;
    void onInit(const muse::IApplication::RunMode& mode) override;
    void onDeinit() override;

private:
    std::shared_ptr<ProcessingController> m_processingController;
    std::shared_ptr<ProcessingUiActions> m_processingUiActions;
};
}

#endif // AU_PROСESSING_PROСESSINGMODULE_H
