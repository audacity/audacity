/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "modularity/imodulesetup.h"

namespace au::effects::extensions {
class ExtensionEffectsModule final : public muse::modularity::IModuleSetup
{
public:
    std::string moduleName() const override;
};
} // namespace au::effects::extensions
