/*
 * Audacity: A Digital Audio Editor
 */
#include "pluginregistrationapp.h"

#include <QCoreApplication>

#include "modularity/ioc.h"
#include "framework/audioplugins/iregisteraudiopluginsscenario.h"
#include "types/ret.h"

#include "log.h"

using namespace muse;
using namespace au::app;

PluginRegistrationApp::PluginRegistrationApp(const std::shared_ptr<AudacityCmdOptions>& options)
    : muse::BaseApplication(options)
{
}

void PluginRegistrationApp::startupScenario(const muse::modularity::ContextPtr& ctxId)
{
    std::shared_ptr<AudacityCmdOptions> options = std::dynamic_pointer_cast<AudacityCmdOptions>(m_appOptions);
    IF_ASSERT_FAILED(options) {
        qApp->exit(1);
        return;
    }

    QMetaObject::invokeMethod(qApp, [this, ctxId, options]() {
        int code = 0;
        if (options->audioPluginRegistration.selfTest) {
            code = runSelfTest(ctxId);
        } else {
            code = processAudioPluginRegistration(ctxId);
        }
        qApp->exit(code);
    }, Qt::QueuedConnection);
}

int PluginRegistrationApp::runSelfTest(const muse::modularity::ContextPtr& ctxId)
{
    LOGI() << "PluginRegistrationApp self-test: initialization successful";

    auto scenario = modularity::ioc(ctxId)->resolve<muse::audioplugins::IRegisterAudioPluginsScenario>("app");
    if (!scenario) {
        LOGE() << "Self-test failed: registerAudioPluginsScenario not available";
        return 1;
    }

    LOGI() << "PluginRegistrationApp self-test: all checks passed";
    return 0;
}

int PluginRegistrationApp::processAudioPluginRegistration(const muse::modularity::ContextPtr& ctxId)
{
    std::shared_ptr<AudacityCmdOptions> options = std::dynamic_pointer_cast<AudacityCmdOptions>(m_appOptions);
    IF_ASSERT_FAILED(options) {
        return 1;
    }

    auto scenario = modularity::ioc(ctxId)->resolve<muse::audioplugins::IRegisterAudioPluginsScenario>("app");
    if (!scenario) {
        LOGE() << "Audio plugin registration scenario not available";
        return 1;
    }

    const auto& task = options->audioPluginRegistration;
    Ret ret = scenario->validatePlugin(task.pluginPath, task.outputPath);

    if (!ret) {
        LOGE() << ret.toString();
    }

    return ret.code();
}
