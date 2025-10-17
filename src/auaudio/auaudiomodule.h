/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>

#include "modularity/imodulesetup.h"

namespace au::auaudio {
class AuAudioModule : public muse::modularity::IModuleSetup
{
public:
    std::string moduleName() const override;
};
}
