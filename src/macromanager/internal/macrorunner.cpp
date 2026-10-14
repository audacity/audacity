/*
* Audacity: A Digital Audio Editor
*/
#include "macrorunner.h"

#include "global/translation.h"
#include "io/path.h"

#include "trackedit/trackeditcommands.h"

#include "log.h"

using namespace au::macromanager;
using namespace muse;
using namespace muse::rcommand;

namespace {
const std::string EXPORT_COMMAND_PATH_PREFIX("importexport/export/");
const std::string EXPORT_NAME_PARAM("name");
constexpr size_t IMPORT_OPERATIONS = 1;

bool isExportCommand(const Command& command)
{
    return command.path().starts_with(EXPORT_COMMAND_PATH_PREFIX);
}
}

async::Promise<Ret> MacroRunner::run(const Macro& macro)
{
    return start(macro, {});
}

async::Promise<Ret> MacroRunner::runOnFiles(const Macro& macro, const io::paths_t& files)
{
    return start(macro, files);
}

void MacroRunner::abort()
{
    m_abortRequested = true;
}

bool MacroRunner::isRunning() const
{
    return m_running;
}

async::Notification MacroRunner::isRunningChanged() const
{
    return m_isRunningChanged;
}

async::Channel<MacroFileProgress> MacroRunner::fileProgress() const
{
    return m_fileProgress;
}

async::Promise<Ret> MacroRunner::start(const Macro& macro, const io::paths_t& files)
{
    return async::make_promise<Ret>([this, macro, files](async::Promise<Ret>::Resolve resolve, async::Promise<Ret>::Reject) {
        if (m_running) {
            return resolve(make_ret(Ret::Code::NotSupported, "a macro is already running"));
        }

        if (!globalContext()->currentProject()) {
            return resolve(make_ret(Ret::Code::NotSupported, "no project is open"));
        }

        if (!files.empty() && !globalContext()->currentTrackeditProject()->trackIdList().empty()) {
            return resolve(make_ret(Ret::Code::NotSupported, trc("macromanager", "Save and close the current project first")));
        }

        m_macro = macro;
        m_files = files;
        m_fileIndex = 0;
        m_abortRequested = false;
        m_resolve = resolve;
        m_startStateIndex = projectHistory()->currentStateIndex();

        m_running = true;
        m_isRunningChanged.notify();

        if (m_files.empty()) {
            runSteps([this](const Ret& ret) { finish(ret); });
        } else {
            runNextFile();
        }

        return async::Promise<Ret>::Result::unchecked();
    });
}

void MacroRunner::runNextFile()
{
    if (m_abortRequested) {
        finish(make_ret(Ret::Code::Cancel));
        return;
    }

    if (m_fileIndex >= m_files.size()) {
        finish(make_ok());
        return;
    }

    sendFileProgress(0);

    const Ret importRet = globalContext()->currentProject()->import(m_files.at(m_fileIndex));
    if (!importRet) {
        finish(importRet);
        return;
    }

    sendFileProgress(IMPORT_OPERATIONS);

    dispatch(trackedit::TRACKEDIT_SELECT_ALL_COMMAND, {}, [this](const Ret& selectRet) {
        if (!selectRet) {
            finish(selectRet);
            return;
        }

        runSteps([this](const Ret& stepsRet) {
            resetProject([this, stepsRet](const Ret& resetRet) {
                if (!stepsRet) {
                    finish(stepsRet);
                    return;
                }

                if (!resetRet) {
                    finish(resetRet);
                    return;
                }

                ++m_fileIndex;
                runNextFile();
            });
        });
    });
}

void MacroRunner::runSteps(const OnDone& onDone)
{
    runStep(0, onDone);
}

void MacroRunner::runStep(size_t index, const OnDone& onDone)
{
    if (m_abortRequested) {
        onDone(make_ret(Ret::Code::Cancel));
        return;
    }

    if (index >= m_macro.steps.size()) {
        onDone(make_ok());
        return;
    }

    const MacroStep& step = m_macro.steps.at(index);
    dispatch(step.command, stepParams(step), [this, index, onDone](const Ret& ret) {
        if (!ret) {
            onDone(ret);
            return;
        }

        sendFileProgress(totalOperations() - m_macro.steps.size() + index + 1);
        runStep(index + 1, onDone);
    });
}

void MacroRunner::resetProject(const OnDone& onDone)
{
    dispatch(trackedit::TRACKEDIT_SELECT_ALL_TRACKS_COMMAND, {}, [this, onDone](const Ret& selectRet) {
        if (!selectRet) {
            onDone(selectRet);
            return;
        }

        dispatch(trackedit::TRACKEDIT_TRACK_DELETE_COMMAND, {}, [this, onDone](const Ret& deleteRet) {
            clipboard()->clearTrackData();
            onDone(deleteRet);
        });
    });
}

void MacroRunner::finish(const Ret& ret)
{
    if (!ret) {
        undoToStartState();
        showError(ret);
    }

    m_running = false;
    m_isRunningChanged.notify();

    (void)m_resolve(ret);
}

void MacroRunner::dispatch(const Command& command, const Params& params, const OnDone& onDone)
{
    commandDispatcher()->dispatch(command, params).onResolve(this, [onDone](const Response& response) {
        onDone(response.ret);
    });
}

Params MacroRunner::stepParams(const MacroStep& step) const
{
    Params params = step.params;

    if (!m_files.empty() && isExportCommand(step.command)) {
        params[EXPORT_NAME_PARAM] = Val(io::completeBasename(m_files.at(m_fileIndex)).toStdString());
    }

    return params;
}

void MacroRunner::sendFileProgress(size_t completedOperations)
{
    MacroFileProgress progress;
    progress.fileIndex = m_fileIndex;
    progress.completedOperations = completedOperations;
    progress.totalOperations = totalOperations();

    m_fileProgress.send(progress);
}

size_t MacroRunner::totalOperations() const
{
    return (m_files.empty() ? 0 : IMPORT_OPERATIONS) + m_macro.steps.size();
}

void MacroRunner::undoToStartState()
{
    if (!m_files.empty()) {
        return;
    }

    projectHistory()->rollbackState();

    while (projectHistory()->currentStateIndex() > m_startStateIndex && projectHistory()->undoAvailable()) {
        projectHistory()->undo();
    }
}

void MacroRunner::showError(const Ret& ret)
{
    if (Ret::Code(ret.code()) == Ret::Code::Cancel) {
        return;
    }

    LOGE() << "macro failed: " << ret.toString();

    interactive()->error(trc("macromanager", "The macro could not be completed"), ret.text(), {},
                         int(IInteractive::Button::NoButton), { IInteractive::Option::WithIcon }, m_macro.name);
}
