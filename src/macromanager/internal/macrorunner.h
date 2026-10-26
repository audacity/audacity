/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/async/asyncable.h"

#include "framework/global/modularity/ioc.h"
#include "framework/interactive/iinteractive.h"
#include "framework/rcommand/icommanddispatcher.h"
#include "framework/toast/itoastservice.h"
#include "context/iglobalcontext.h"
#include "trackedit/iprojecthistory.h"
#include "trackedit/itrackeditclipboard.h"
#include "trackedit/itrackeditinteraction.h"

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
    muse::ContextInject<trackedit::ITrackeditInteraction> trackeditInteraction{ this };

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
    muse::Ret begin(const Macro& macro);
    void finish(const Macro& macro, const muse::Ret& ret);

    muse::async::Promise<muse::Ret> runFiles(const Macro& macro, const muse::io::paths_t& files);
    muse::async::Promise<muse::Ret> runFile(const Macro& macro, const muse::io::path_t& file);
    muse::async::Promise<muse::Ret> runSteps(const std::vector<MacroItem>& items);
    muse::async::Promise<muse::Ret> runItem(const MacroItem& item);
    muse::async::Promise<muse::Ret> runMacro(const MacroReference& reference);
    muse::async::Promise<muse::Ret> resetProject();

    muse::rcommand::Params stepParams(const MacroStep& step) const;
    void undoToState(size_t stateIndex);
    void showCompleted(const std::string& macroName);
    void showError(const muse::Ret& ret, const std::string& macroName);

    std::vector<std::string> m_outputNames;
    std::vector<muse::Ret> m_fileResults;
    size_t m_appliedStepCount = 0;
    bool m_running = false;
    bool m_abortRequested = false;

    muse::async::Notification m_isRunningChanged;
    muse::async::Notification m_currentFileChanged;
};
}
