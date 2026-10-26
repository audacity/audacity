/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <functional>
#include <memory>

#include "framework/global/async/asyncable.h"

#include "framework/global/modularity/ioc.h"
#include "framework/interactive/iinteractive.h"
#include "framework/rcommand/icommanddispatcher.h"
#include "framework/toast/itoastservice.h"
#include "context/iglobalcontext.h"
#include "trackedit/iprojecthistory.h"
#include "trackedit/itrackeditclipboard.h"

#include "macromanager/imacrorunner.h"
#include "macromanager/imacrosrepository.h"

namespace au::macromanager {
class MacroRunner : public IMacroRunner, public muse::Contextable, public muse::async::Asyncable
{
    muse::GlobalInject<muse::toast::IToastService> toastService;
    muse::GlobalInject<IMacrosRepository> repository;

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

    size_t currentFileIndex() const override;
    std::vector<muse::Ret> fileResults() const override;
    muse::async::Notification currentFileChanged() const override;

private:
    using OnDone = std::function<void (const muse::Ret&)>;
    using Steps = std::shared_ptr<const std::vector<MacroItem> >;

    muse::async::Promise<muse::Ret> start(const Macro& macro, const muse::io::paths_t& files);

    void runNextFile();
    void finishFile(const muse::Ret& ret);
    void runSteps(const std::vector<MacroItem>& steps, const OnDone& onDone);
    void runStep(const Steps& steps, size_t index, const OnDone& onDone);
    void runMacro(const MacroReference& reference, const OnDone& onDone);
    void resetProject(const OnDone& onDone);
    void finish(const muse::Ret& ret);

    void dispatch(const muse::rcommand::Command& command, const muse::rcommand::Params& params, const OnDone& onDone);
    muse::rcommand::Params stepParams(const MacroStep& step) const;
    void undoToStartState();
    void showCompleted();
    void showError(const muse::Ret& ret);

    Macro m_macro;
    std::vector<std::string> m_macroStack;
    muse::io::paths_t m_files;
    std::vector<std::string> m_outputNames;
    size_t m_fileIndex = 0;
    size_t m_appliedStepCount = 0;
    std::vector<muse::Ret> m_fileResults;
    size_t m_startStateIndex = 0;
    bool m_running = false;
    bool m_abortRequested = false;
    muse::async::Promise<muse::Ret>::Resolve m_resolve;

    muse::async::Notification m_isRunningChanged;
    muse::async::Notification m_currentFileChanged;
};
}
