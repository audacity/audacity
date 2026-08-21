/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"
#include "framework/testflow/itestflowconfiguration.h"

#include "cmdoptions.h"

namespace au::app {
//! NOTE Runs one test case, then ends the process with its result
class TestflowRunner : public muse::async::Asyncable
{
    muse::GlobalInject<muse::testflow::ITestflowConfiguration> configuration;

public:
    static void runIfRequested(const muse::modularity::ContextPtr& ctx, const AudacityCmdOptions::Testflow& options);

private:
    TestflowRunner(const muse::modularity::ContextPtr& ctx, const AudacityCmdOptions::Testflow& options);

    void run();
    void execAndReport(const muse::io::path_t& scriptPath);
    muse::io::path_t resolveScriptPath() const;

    muse::modularity::ContextPtr m_ctx;
    AudacityCmdOptions::Testflow m_options;
    int m_startedSteps = 0;
};
}
