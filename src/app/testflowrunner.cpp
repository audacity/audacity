/*
 * Audacity: A Digital Audio Editor
 */
#include "testflowrunner.h"

#include <cstdlib>
#include <iostream>

#include <QCoreApplication>
#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QRegularExpression>
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

    const muse::io::path_t scriptPath = resolveScriptPath(options.testCaseNameOrFile);
    const ScriptDirectives directives = readDirectives(scriptPath);

    au::appshell::installTestflowStartupScenario(ctx, directives.allowRecovery);

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
    (new TestflowRunner(ctx, options, resolveScriptPath(options.testCaseNameOrFile)))->run();
}

TestflowRunner::TestflowRunner(const muse::modularity::ContextPtr& ctx, const AudacityCmdOptions::Testflow& options,
                               const muse::io::path_t& scriptPath)
    : m_ctx(ctx), m_options(options), m_scriptPath(scriptPath)
{
}

TestflowRunner::ScriptDirectives TestflowRunner::readDirectives(const muse::io::path_t& scriptPath)
{
    QFile file(scriptPath.toQString());
    if (!file.open(QIODevice::ReadOnly | QIODevice::Text)) {
        std::cout << "[testflow] FAILED: cannot read script: " << scriptPath.toStdString() << std::endl;
        exitWithoutTeardown(2);
    }

    ScriptDirectives directives;

    //! NOTE Like Jest's pragmas, only the docblock the script starts with counts
    static const QRegularExpression docblock(R"(^\s*/\*.*?\*/)", QRegularExpression::DotMatchesEverythingOption);
    const QRegularExpressionMatch header = docblock.match(QString::fromUtf8(file.readAll()));
    if (!header.hasMatch()) {
        return directives;
    }

    static const QRegularExpression tag(R"(@testflow\s+(\S+))");
    QRegularExpressionMatchIterator it = tag.globalMatch(header.captured());
    while (it.hasNext()) {
        const QString directive = it.next().captured(1);
        if (directive == "allow-recovery") {
            directives.allowRecovery = true;
        } else {
            //! NOTE Rather than letting a typo make the test case fail for another reason
            std::cout << "[testflow] FAILED: unknown directive @testflow " << directive.toStdString()
                      << " in script: " << scriptPath.toStdString() << std::endl;
            exitWithoutTeardown(2);
        }
    }

    return directives;
}

muse::io::path_t TestflowRunner::resolveScriptPath(const QString& nameOrFile)
{
    if (QFileInfo::exists(nameOrFile)) {
        return muse::io::path_t(QFileInfo(nameOrFile).absoluteFilePath());
    }

    const auto configuration = muse::modularity::globalIoc()->resolve<muse::testflow::ITestflowConfiguration>("app");
    IF_ASSERT_FAILED(configuration) {
        exitWithoutTeardown(2);
    }

    for (const muse::io::path_t& dir : configuration->scriptsDirPaths()) {
        for (const QString& candidate : { nameOrFile, nameOrFile + ".js" }) {
            const QString path = QDir::cleanPath(dir.toQString() + "/" + candidate);
            if (QFileInfo::exists(path)) {
                return muse::io::path_t(path);
            }
        }
    }

    std::cout << "[testflow] FAILED: script not found: " << nameOrFile.toStdString() << std::endl;
    exitWithoutTeardown(2);
}

void TestflowRunner::run()
{
    std::cout << "[testflow] running script=" << m_scriptPath.toStdString() << std::endl;

    QTimer::singleShot(SETTLE_MS, qApp, [this]() {
        execAndReport();
    });
}

void TestflowRunner::execAndReport()
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

    testflow->execScript(m_scriptPath, opt);

    const muse::testflow::ITestflow::Status status = testflow->status();
    const bool ok = status == muse::testflow::ITestflow::Status::Finished && m_startedSteps > 0;

    std::cout << "[testflow] " << (ok ? "PASSED" : "FAILED")
              << " status=" << muse::testflow::ITestflow::statusToString(status).toStdString()
              << " steps=" << m_startedSteps
              << " script=" << m_scriptPath.toStdString()
              << " reports=" << configuration()->reportsPath().toStdString() << std::endl;
    if (m_startedSteps == 0) {
        std::cout << "[testflow] no steps were executed"
                  << " - does the script define main() and run a test case?" << std::endl;
    }

    exitWithoutTeardown(ok ? 0 : 1);
}
