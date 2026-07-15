/*
 * Audacity: A Digital Audio Editor
 */
#include "audacitypluginmodule.h"

#include "framework/global/modularity/ioc.h"

#include "iaudacitypluginhost.h"
#include "internal/audacitypluginhost.h"

namespace au::audacityplugin {
namespace {
const std::string mname("audacityplugin");
}

std::string AudacityPluginModule::moduleName() const
{
    return mname;
}

void AudacityPluginModule::registerExports()
{
    m_host = std::make_shared<AudacityPluginHost>();
    globalIoc()->registerExport<IAudacityPluginHost>(mname, m_host);
}

void AudacityPluginModule::onPreInit(const muse::IApplication::RunMode&)
{
    m_host->initialize();
}

void AudacityPluginModule::onDeinit()
{
    m_host->shutdown();
}
} // namespace au::audacityplugin
