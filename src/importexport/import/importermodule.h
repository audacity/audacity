/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "modularity/imodulesetup.h"

namespace au::importexport {
class Au3Importer;
class ImporterModule : public muse::modularity::IModuleSetup
{
public:
    ImporterModule();

    std::string moduleName() const override;
    void registerExports() override;

    void onInit(const muse::IApplication::RunMode& mode) override;

private:
    std::shared_ptr<Au3Importer> m_importer;
};
}
