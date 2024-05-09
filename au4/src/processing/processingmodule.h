/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_PROСESSING_PROСESSINGMODULE_H
#define AU_PROСESSING_PROСESSINGMODULE_H

#include "modularity/imodulesetup.h"

namespace au::processing {
class ProcessingModule : public muse::modularity::IModuleSetup
{
public:

    std::string moduleName() const override;
    void registerExports() override;
    void onInit(const muse::IApplication::RunMode& mode) override;
    void onDeinit() override;

private:
};
}

#endif // AU_PROСESSING_PROСESSINGMODULE_H
