/*
 * Audacity: A Digital Audio Editor
 */
#ifndef AU_APP_PLUGINREGISTRATIONAPP_H
#define AU_APP_PLUGINREGISTRATIONAPP_H

#include <QList>
#include <memory>
#include <vector>

#include "modularity/imodulesetup.h"
#include "global/internal/baseapplication.h"
#include "global/globalmodule.h"
#include "modularity/ioc.h"
#include "audioplugins/iregisteraudiopluginsscenario.h"

#include "commandlineparser.h"

namespace au::app {
class PluginRegistrationApp : public muse::BaseApplication, public std::enable_shared_from_this<PluginRegistrationApp>
{
    muse::GlobalInject<muse::audioplugins::IRegisterAudioPluginsScenario> registerAudioPluginsScenario;

public:
    PluginRegistrationApp(const CommandLineParser::AudioPluginRegistration& task, const muse::modularity::ContextPtr& ctx);

    void addModule(muse::modularity::IModuleSetup* module);

    void setup() override;
    void finish() override;

    muse::modularity::ContextPtr setupNewContext(const muse::StringList& args = {}) override;
    void destroyContext(const muse::modularity::ContextPtr& ctx) override;
    size_t contextCount() const override;
    std::vector<muse::modularity::ContextPtr> contexts() const override;

private:
    int runSelfTest();
    int processAudioPluginRegistration();
    std::vector<muse::modularity::IContextSetup*>& contextSetups(const muse::modularity::ContextPtr& ctx);

    CommandLineParser::AudioPluginRegistration m_task;
    muse::GlobalModule m_globalModule;
    QList<muse::modularity::IModuleSetup*> m_modules;

    struct Context {
        muse::modularity::ContextPtr ctx;
        std::vector<muse::modularity::IContextSetup*> setups;
    };

    std::vector<Context> m_contexts;
};
}

#endif // AU_APP_PLUGINREGISTRATIONAPP_H
