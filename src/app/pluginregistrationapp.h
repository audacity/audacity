/*
 * Audacity: A Digital Audio Editor
 */
#ifndef AU_APP_PLUGINREGISTRATIONAPP_H
#define AU_APP_PLUGINREGISTRATIONAPP_H

#include <memory>

#include "global/internal/baseapplication.h"

#include "cmdoptions.h"

namespace au::app {
class PluginRegistrationApp : public muse::BaseApplication
{
public:
    PluginRegistrationApp(const std::shared_ptr<AudacityCmdOptions>& options);

protected:
    void startupScenario(const muse::modularity::ContextPtr& ctxId) override;

private:
    int runSelfTest(const muse::modularity::ContextPtr& ctxId);
    int processAudioPluginRegistration(const muse::modularity::ContextPtr& ctxId);
};
}

#endif // AU_APP_PLUGINREGISTRATIONAPP_H
