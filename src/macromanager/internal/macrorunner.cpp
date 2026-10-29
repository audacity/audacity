/*
* Audacity: A Digital Audio Editor
*/
#include "macrorunner.h"

#include <chrono>

#include "framework/global/translation.h"
#include "framework/global/io/path.h"
#include "framework/global/log.h"

#include "importexport/export/exportcommands.h"
#include "trackedit/trackeditcommands.h"

#include "macrooutputnames.h"

using namespace au::macromanager;
using namespace muse;
using namespace muse::rcommand;

namespace {
constexpr std::chrono::seconds COMPLETED_TOAST_TIMEOUT(5);

async::Promise<Ret> resolved(const Ret& ret)
{
    return async::make_promise<Ret>([ret](auto resolve) {
        return resolve(ret);
    });
}
}

async::Promise<Ret> MacroRunner::run(const Macro& macro)
{
    if (const Ret ready = begin(macro); !ready) {
        return resolved(ready);
    }

    const size_t startStateIndex = projectHistory()->currentStateIndex();

    return runSteps(macro.steps).then<Ret>(this, [this, macro, startStateIndex](const Ret& ret, auto resolve) {
        if (ret) {
            showCompleted(macro.name);
        } else {
            undoToState(startStateIndex);
        }

        finish(macro, ret);
        return resolve(ret);
    });
}

async::Promise<Ret> MacroRunner::runOnFiles(const Macro& macro, const io::paths_t& files)
{
    if (const Ret ready = begin(macro); !ready) {
        return resolved(ready);
    }

    m_outputNames = uniqueOutputNames(files);

    return runFiles(macro, files).then<Ret>(this, [this, macro](const Ret& ret, auto resolve) {
        finish(macro, ret);
        return resolve(ret);
    });
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

size_t MacroRunner::currentFileIndex() const
{
    return m_fileResults.size();
}

std::vector<Ret> MacroRunner::fileResults() const
{
    return m_fileResults;
}

async::Notification MacroRunner::currentFileChanged() const
{
    return m_currentFileChanged;
}

Ret MacroRunner::begin(const Macro& macro)
{
    if (m_running) {
        return make_ret(Ret::Code::NotSupported, "a macro is already running");
    }

    if (!globalContext()->currentProject()) {
        return make_ret(Ret::Code::NotSupported, "no project is open");
    }

    const Ret valid = repository()->validateReferences(macro);
    if (!valid) {
        showError(valid, macro.name);
        return valid;
    }

    m_outputNames.clear();
    m_fileResults.clear();
    m_appliedStepCount = 0;
    m_abortRequested = false;
    m_running = true;
    m_isRunningChanged.notify();

    return make_ok();
}

void MacroRunner::finish(const Macro& macro, const Ret& ret)
{
    if (!ret) {
        showError(ret, macro.name);
    }

    m_running = false;
    m_isRunningChanged.notify();
}

async::Promise<Ret> MacroRunner::runFiles(const Macro& macro, const io::paths_t& files)
{
    async::Promise<Ret> chain = resolved(make_ok());

    for (const io::path_t& file : files) {
        chain = chain.then<Ret>(this, [this, macro, file](const Ret& previous, auto resolve) {
            if (!previous || m_abortRequested || !globalContext()->currentProject()) {
                return resolve(make_ret(Ret::Code::Cancel));
            }

            m_currentFileChanged.notify();

            runFile(macro, file).onResolve(this, [this, file, resolve](const Ret& ret) {
                if (!ret) {
                    LOGE() << "macro failed on file " << file << ": " << ret.toString();
                }

                m_fileResults.push_back(ret);
                (void)resolve(make_ok());
            });

            return async::Promise<Ret>::dummy_result();
        });
    }

    return chain;
}

async::Promise<Ret> MacroRunner::runFile(const Macro& macro, const io::path_t& file)
{
    const Ret importRet = globalContext()->currentProject()->import(file);
    async::Promise<Ret> stepsRun = importRet ? runSteps(macro.steps) : resolved(importRet);

    return stepsRun.then<Ret>(this, [this](const Ret& stepsRet, auto resolve) {
        resetProject().onResolve(this, [stepsRet, resolve](const Ret& resetRet) {
            (void)resolve(stepsRet ? resetRet : stepsRet);
        });

        return async::Promise<Ret>::dummy_result();
    });
}

async::Promise<Ret> MacroRunner::runSteps(const std::vector<MacroItem>& items)
{
    async::Promise<Ret> chain = resolved(make_ok());

    for (const MacroItem& item : items) {
        chain = chain.then<Ret>(this, [this, item](const Ret& previous, auto resolve) {
            if (!previous) {
                return resolve(previous);
            }

            if (m_abortRequested) {
                return resolve(make_ret(Ret::Code::Cancel));
            }

            runItem(item).onResolve(this, [resolve](const Ret& ret) {
                (void)resolve(ret);
            });

            return async::Promise<Ret>::dummy_result();
        });
    }

    return chain;
}

async::Promise<Ret> MacroRunner::runItem(const MacroItem& item)
{
    const MacroStep* step = std::get_if<MacroStep>(&item);
    if (!step) {
        return runMacro(std::get<MacroReference>(item));
    }

    return commandDispatcher()->dispatch(step->command, stepParams(*step))
           .then<Ret>(this, [this](const Response& response, auto resolve) {
        if (response.ret) {
            ++m_appliedStepCount;
        }

        return resolve(response.ret);
    });
}

async::Promise<Ret> MacroRunner::runMacro(const MacroReference& reference)
{
    const std::optional<Macro> macro = repository()->macro(reference.name);
    if (!macro) {
        const QString name = QString::fromStdString(reference.name);
        return resolved(make_ret(Ret::Code::NotSupported, qtrc("macromanager", "Macro “%1” not found").arg(name).toStdString()));
    }

    return runSteps(macro->steps);
}

async::Promise<Ret> MacroRunner::resetProject()
{
    return commandDispatcher()->dispatch(trackedit::TRACKEDIT_SELECT_ALL_TRACKS_COMMAND)
           .then<Ret>(this, [this](const Response& selectResponse, auto resolve) {
        if (!selectResponse.ret) {
            return resolve(selectResponse.ret);
        }

        commandDispatcher()->dispatch(trackedit::TRACKEDIT_TRACK_DELETE_COMMAND)
        .onResolve(this, [this, resolve](const Response& deleteResponse) {
            clipboard()->clearTrackData();
            (void)resolve(deleteResponse.ret);
        });

        return async::Promise<Ret>::dummy_result();
    });
}

Params MacroRunner::stepParams(const MacroStep& step) const
{
    Params params = step.params;

    if (!m_outputNames.empty() && importexport::isExportCommand(step.command)) {
        params[importexport::EXPORT_NAME_PARAM] = Val(m_outputNames.at(m_fileResults.size()));
    }

    return params;
}

void MacroRunner::undoToState(size_t stateIndex)
{
    projectHistory()->rollbackState();
    globalContext()->currentTrackeditProject()->reload();

    trackeditInteraction()->undoRedoToIndex(stateIndex);
}

void MacroRunner::showCompleted(const std::string& macroName)
{
    const QString name = QString::fromStdString(macroName);
    const QString title = qtrc("macromanager", "“%1” ran successfully").arg(name);
    const QString result = qtrc("macromanager", "%n step(s) applied to the project", nullptr, int(m_appliedStepCount));
    const std::string message = QString("\n%1\n%2\n\n").arg(title, result).toStdString();

    toastService()->showWithTimeout(trc("macromanager", "Macro completed"), message, COMPLETED_TOAST_TIMEOUT,
                                    ui::IconCode::Code::TICK);
}

void MacroRunner::showError(const Ret& ret, const std::string& macroName)
{
    if (Ret::Code(ret.code()) == Ret::Code::Cancel) {
        return;
    }

    LOGE() << "macro failed: " << ret.toString();

    interactive()->error(trc("macromanager", "The macro could not be completed"), ret.text(), {},
                         int(IInteractive::Button::NoButton), { IInteractive::Option::WithIcon }, macroName);
}
