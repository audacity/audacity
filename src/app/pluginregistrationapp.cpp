/*
 * Audacity: A Digital Audio Editor
 */
#include "pluginregistrationapp.h"

#include <QCoreApplication>

#include "backgroundprocess.h"
#include "modularity/ioc.h"
#include "framework/audioplugins/iregisteraudiopluginsscenario.h"
#include "types/ret.h"

#include "log.h"

using namespace muse;
using namespace au::app;

PluginRegistrationApp::PluginRegistrationApp(const std::shared_ptr<AudacityCmdOptions>& options)
    : muse::BaseApplication(options)
{
    makeProcessBackground();
}

void PluginRegistrationApp::applyCommandLineOptions(const std::shared_ptr<muse::CmdOptions>& options)
{
    BaseApplication::applyCommandLineOptions(options);

    if (!crashHandler()) {
        // By default only available on non-development CI builds.
        return;
    }

    const auto audacityOptions = std::dynamic_pointer_cast<AudacityCmdOptions>(options);
    IF_ASSERT_FAILED(audacityOptions) {
        return;
    }

    const auto& task = audacityOptions->audioPluginRegistration;
    if (task.pluginPath.empty()) {
        LOGE() << "plugin path arg not provided";
        return;
    }

    // Only keep the plugin name: the full path could reveal the user's name or folder layout
    crashHandler()->addSessionTag(muse::String{"plugin-validation"}, io::filename(task.pluginPath).toString());
}

void PluginRegistrationApp::startupScenario(const muse::modularity::ContextPtr& ctxId)
{
    std::shared_ptr<AudacityCmdOptions> options = std::dynamic_pointer_cast<AudacityCmdOptions>(m_appOptions);
    IF_ASSERT_FAILED(options) {
        qApp->exit(1);
        return;
    }

    // Keep the system crash reporter out of it plugin registration: on macOS it would show
    // one "Audacity quit unexpectedly" dialog per crashing plugin. The dump for our own
    // crash server is still written.
    if (crashHandler()) {
        crashHandler()->setSystemCrashReporterForwardingEnabled(false);
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
