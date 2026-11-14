/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "modularity/imodulesetup.h"

namespace au::extensions {
class AudacityExtensionsModule final : public muse::modularity::IModuleSetup
{
public:
    std::string moduleName() const override;
};
} // namespace au::extensions
