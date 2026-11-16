/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/imodulesetup.h"

namespace au::musehub {
class MuseHubModule : public muse::modularity::IModuleSetup
{
public:
    std::string moduleName() const override;
};
}
