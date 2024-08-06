/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_RECORD_RECORDMODULE_H
#define AU_RECORD_RECORDMODULE_H

#include <memory>

#include "modularity/imodulesetup.h"

namespace au::record {
class RecordConfiguration;
class RecordController;
class RecordUiActions;
class Au3Record;
class RecordModule : public muse::modularity::IModuleSetup
{
public:

    std::string moduleName() const override;
    void registerExports() override;
    void resolveImports() override;
    void registerResources() override;
    void onInit(const muse::IApplication::RunMode& mode) override;
    void onDeinit() override;

private:
    std::shared_ptr<RecordConfiguration> m_configuration;
    std::shared_ptr<RecordController> m_controller;
    std::shared_ptr<RecordUiActions> m_uiActions;
    std::shared_ptr<Au3Record> m_record;
};
}

#endif // AU_RECORD_RECORDMODULE_H
