/*
 * Audacity: A Digital Audio Editor
 */
#include "testflowrunner.h"

#include <cstdlib>
#include <iostream>

#include <QCoreApplication>
#include <QDir>
#include <QFileInfo>
#include <QTimer>

#include "framework/testflow/itestflow.h"

#include "appshell/testflowstartup.h"
#include "effects/effects_base/ieffectsproviderinitializer.h"

#include "log.h"

using namespace au::app;

//! NOTE No signal says the UI is ready: startupCompleted() is set before the
//! startup page even opens, so the first step just has to wait
static constexpr int SETTLE_MS = 3000;

namespace {
//! NOTE Skips teardown, where actions queued by the last steps crash on
//! late-rejected Interactive::openSync promises. Flush your output first.
[[noreturn]] void exitWithoutTeardown(int code)
{
    std::_Exit(code);
}
}

void TestflowRunner::prepare(const muse::modularity::ContextPtr& ctx, const AudacityCmdOptions::Testflow& options)
{
    if (options.testCaseNameOrFile.isEmpty()) {
        return;
    }

    au::appshell::installTestflowStartupScenario(ctx);

    auto effectsInitializer = muse::modularity::ioc(ctx)->resolve<au::effects::IEffectsProviderInitializer>("app");
    IF_ASSERT_FAILED(effectsInitializer) {
        return;
    }
    effectsInitializer->setStartupPluginValidationPolicy(au::effects::StartupPluginValidationPolicy::Skip);
}

void TestflowRunner::runIfRequested(const muse::modularity::ContextPtr& ctx, const AudacityCmdOptions::Testflow& options)
{
    if (options.testCaseNameOrFile.isEmpty()) {
        return;
    }

    //! NOTE Lives until the process ends, which it does itself
    (new TestflowRunner(ctx, options))->run();
}

TestflowRunner::TestflowRunner(const muse::modularity::ContextPtr& ctx, const AudacityCmdOptions::Testflow& options)
    : m_ctx(ctx), m_options(options)
{
}

muse::io::path_t TestflowRunner::resolveScriptPath() const
{
    const QString& nameOrFile = m_options.testCaseNameOrFile;
    if (QFileInfo::exists(nameOrFile)) {
        return muse::io::path_t(QFileInfo(nameOrFile).absoluteFilePath());
    }

    for (const muse::io::path_t& dir : configuration()->scriptsDirPaths()) {
        for (const QString& candidate : { nameOrFile, nameOrFile + ".js" }) {
            const QString path = QDir::cleanPath(dir.toQString() + "/" + candidate);
            if (QFileInfo::exists(path)) {
                return muse::io::path_t(path);
            }
        }
    }

    return {};
}

void TestflowRunner::run()
{
    const muse::io::path_t scriptPath = resolveScriptPath();
    if (scriptPath.empty()) {
        std::cout << "[testflow] FAILED: script not found: " << m_options.testCaseNameOrFile.toStdString() << std::endl;
        exitWithoutTeardown(2);
    }

    std::cout << "[testflow] running script=" << scriptPath.toStdString() << std::endl;

    QTimer::singleShot(SETTLE_MS, qApp, [this, scriptPath]() {
        execAndReport(scriptPath);
    });
}

void TestflowRunner::execAndReport(const muse::io::path_t& scriptPath)
{
    auto testflow = muse::modularity::ioc(m_ctx)->resolve<muse::testflow::ITestflow>("app");
    IF_ASSERT_FAILED(testflow) {
        exitWithoutTeardown(2);
    }

    if (!m_options.testCaseSpeed.isEmpty()) {
        testflow->setSpeedMode(muse::testflow::speedModeFromString(m_options.testCaseSpeed));
    }

    muse::testflow::ITestflow::Options opt;
    opt.context = muse::io::path_t(m_options.testCaseContextNameOrFile);
    opt.contextVal = m_options.testCaseContextValue.toStdString();
    opt.func = m_options.testCaseFunc.toStdString();
    opt.funcArgs = m_options.testCaseFuncArgs.toStdString();

    //! NOTE A script that fails to load, or never runs a test case, still ends
    //! up Finished, so count the steps to tell that apart
    testflow->stepStatusChanged().onReceive(this, [this](const muse::testflow::StepInfo& step, const muse::Ret&) {
        if (step.status == muse::testflow::StepStatus::Started) {
            ++m_startedSteps;
        }
    });

    testflow->execScript(scriptPath, opt);

    const muse::testflow::ITestflow::Status status = testflow->status();
    const bool ok = status == muse::testflow::ITestflow::Status::Finished && m_startedSteps > 0;

    std::cout << "[testflow] " << (ok ? "PASSED" : "FAILED")
              << " status=" << muse::testflow::ITestflow::statusToString(status).toStdString()
              << " steps=" << m_startedSteps
              << " script=" << scriptPath.toStdString()
              << " reports=" << configuration()->reportsPath().toStdString() << std::endl;
    if (m_startedSteps == 0) {
        std::cout << "[testflow] no steps were executed"
                  << " - does the script define main() and run a test case?" << std::endl;
    }

    exitWithoutTeardown(ok ? 0 : 1);
}
