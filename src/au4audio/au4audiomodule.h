/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <memory>

#include "framework/global/modularity/imodulesetup.h"

namespace au::au4audio {
class Au4AudioEngine;

class Au4AudioModule : public muse::modularity::IModuleSetup
{
public:
    std::string moduleName() const override;
    void resolveImports() override;

private:
    std::shared_ptr<Au4AudioEngine> m_portAudioEngine;
};
}
