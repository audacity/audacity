/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <memory>

#include "modularity/imodulesetup.h"

namespace au::audacityplugin {
class AudacityPluginHost;

class AudacityPluginModule final : public muse::modularity::IModuleSetup
{
public:
    std::string moduleName() const override;
    void registerExports() override;
    void onPreInit(const muse::IApplication::RunMode& mode) override;
    void onDeinit() override;

private:
    std::shared_ptr<AudacityPluginHost> m_host;
};
} // namespace au::audacityplugin
