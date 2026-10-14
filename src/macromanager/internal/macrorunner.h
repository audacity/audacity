/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <functional>

#include "modularity/ioc.h"
#include "async/asyncable.h"
#include "interactive/iinteractive.h"
#include "rcommand/icommanddispatcher.h"

#include "context/iglobalcontext.h"
#include "trackedit/iprojecthistory.h"
#include "trackedit/itrackeditclipboard.h"

#include "macromanager/imacrorunner.h"

namespace au::macromanager {
class MacroRunner : public IMacroRunner, public muse::Contextable, public muse::async::Asyncable
{
    muse::ContextInject<muse::rcommand::ICommandDispatcher> commandDispatcher{ this };
    muse::ContextInject<muse::IInteractive> interactive{ this };
    muse::ContextInject<context::IGlobalContext> globalContext{ this };
    muse::ContextInject<trackedit::IProjectHistory> projectHistory{ this };
    muse::ContextInject<trackedit::ITrackeditClipboard> clipboard{ this };

public:
    explicit MacroRunner(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    muse::async::Promise<muse::Ret> run(const Macro& macro) override;
    muse::async::Promise<muse::Ret> runOnFiles(const Macro& macro, const muse::io::paths_t& files) override;
    void abort() override;

    bool isRunning() const override;
    muse::async::Notification isRunningChanged() const override;
    muse::async::Channel<MacroFileProgress> fileProgress() const override;

private:
    using OnDone = std::function<void (const muse::Ret&)>;

    muse::async::Promise<muse::Ret> start(const Macro& macro, const muse::io::paths_t& files);

    void runNextFile();
    void runSteps(const OnDone& onDone);
    void runStep(size_t index, const OnDone& onDone);
    void resetProject(const OnDone& onDone);
    void finish(const muse::Ret& ret);

    void dispatch(const muse::rcommand::Command& command, const muse::rcommand::Params& params, const OnDone& onDone);
    muse::rcommand::Params stepParams(const MacroStep& step) const;
    void sendFileProgress(size_t completedOperations);
    size_t totalOperations() const;
    void undoToStartState();
    void showError(const muse::Ret& ret);

    Macro m_macro;
    muse::io::paths_t m_files;
    size_t m_fileIndex = 0;
    size_t m_startStateIndex = 0;
    bool m_running = false;
    bool m_abortRequested = false;
    muse::async::Promise<muse::Ret>::Resolve m_resolve;

    muse::async::Notification m_isRunningChanged;
    muse::async::Channel<MacroFileProgress> m_fileProgress;
};
}
