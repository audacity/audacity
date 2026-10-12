/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"
#include "framework/testflow/itestflowconfiguration.h"

#include "cmdoptions.h"

namespace au::app {
//! NOTE Runs one test case, then ends the process with its result.
//!
//! A script can set how the app starts with `@testflow <directive>` tags in the comment it starts with:
//!  - `allow-recovery`: offer to recover the projects left open by a previous run, as a normal startup would
class TestflowRunner : public muse::async::Asyncable
{
    muse::GlobalInject<muse::testflow::ITestflowConfiguration> configuration;

public:
    //! NOTE Call before the startup scenario is resolved
    static void prepare(const muse::modularity::ContextPtr& ctx, const AudacityCmdOptions::Testflow& options);
    static void runIfRequested(const muse::modularity::ContextPtr& ctx, const AudacityCmdOptions::Testflow& options);

private:
    struct ScriptDirectives {
        bool allowRecovery = false;
    };

    TestflowRunner(const muse::modularity::ContextPtr& ctx, const AudacityCmdOptions::Testflow& options,
                   const muse::io::path_t& scriptPath);

    void run();
    void execAndReport();

    static muse::io::path_t resolveScriptPath(const QString& nameOrFile);
    static ScriptDirectives readDirectives(const muse::io::path_t& scriptPath);

    muse::modularity::ContextPtr m_ctx;
    AudacityCmdOptions::Testflow m_options;
    muse::io::path_t m_scriptPath;
    int m_startedSteps = 0;
};
}
