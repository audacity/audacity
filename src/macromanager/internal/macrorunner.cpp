/*
* Audacity: A Digital Audio Editor
*/
#include "macrorunner.h"

#include <algorithm>
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

size_t MacroRunner::currentFileIndex() const
{
    return m_fileIndex;
}

std::vector<Ret> MacroRunner::fileResults() const
{
    return m_fileResults;
}

async::Notification MacroRunner::currentFileChanged() const
{
    return m_currentFileChanged;
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

        m_macro = macro;

        const Ret valid = repository()->validateReferences(macro);
        if (!valid) {
            showError(valid);
            return resolve(valid);
        }

        m_macroStack = { macro.name };
        m_files = files;
        m_outputNames = uniqueOutputNames(files);
        m_fileIndex = 0;
        m_appliedStepCount = 0;
        m_fileResults.clear();
        m_abortRequested = false;
        m_resolve = resolve;
        m_startStateIndex = projectHistory()->currentStateIndex();

        m_running = true;
        m_isRunningChanged.notify();

        if (m_files.empty()) {
            runSteps(m_macro.steps, [this](const Ret& ret) { finish(ret); });
        } else {
            runNextFile();
        }

        return async::Promise<Ret>::Result::unchecked();
    });
}

void MacroRunner::runNextFile()
{
    if (m_abortRequested || !globalContext()->currentProject()) {
        finish(make_ret(Ret::Code::Cancel));
        return;
    }

    if (m_fileIndex >= m_files.size()) {
        finish(make_ok());
        return;
    }

    m_currentFileChanged.notify();

    const Ret importRet = globalContext()->currentProject()->import(m_files.at(m_fileIndex));
    if (!importRet) {
        resetProject([this, importRet](const Ret&) {
            finishFile(importRet);
        });
        return;
    }

    runSteps(m_macro.steps, [this](const Ret& stepsRet) {
        resetProject([this, stepsRet](const Ret& resetRet) {
            finishFile(stepsRet ? resetRet : stepsRet);
        });
    });
}

void MacroRunner::finishFile(const Ret& ret)
{
    if (!ret) {
        LOGE() << "macro failed on file " << m_files.at(m_fileIndex) << ": " << ret.toString();
    }

    m_fileResults.push_back(ret);
    ++m_fileIndex;
    runNextFile();
}

void MacroRunner::runSteps(const std::vector<MacroItem>& steps, const OnDone& onDone)
{
    runStep(std::make_shared<const std::vector<MacroItem> >(steps), 0, onDone);
}

void MacroRunner::runStep(const Steps& steps, size_t index, const OnDone& onDone)
{
    if (m_abortRequested) {
        onDone(make_ret(Ret::Code::Cancel));
        return;
    }

    if (index >= steps->size()) {
        onDone(make_ok());
        return;
    }

    const OnDone next = [this, steps, index, onDone](const Ret& ret) {
        if (!ret) {
            onDone(ret);
            return;
        }

        runStep(steps, index + 1, onDone);
    };

    const MacroItem& item = steps->at(index);
    if (const MacroStep* step = std::get_if<MacroStep>(&item)) {
        dispatch(step->command, stepParams(*step), [this, next](const Ret& ret) {
            if (ret) {
                ++m_appliedStepCount;
            }

            next(ret);
        });
    } else {
        runMacro(std::get<MacroReference>(item), next);
    }
}

void MacroRunner::runMacro(const MacroReference& reference, const OnDone& onDone)
{
    const QString name = QString::fromStdString(reference.name);

    if (std::ranges::find(m_macroStack, reference.name) != m_macroStack.end()) {
        onDone(make_ret(Ret::Code::NotSupported, qtrc("macromanager", "Macro “%1” calls itself").arg(name).toStdString()));
        return;
    }

    const std::optional<Macro> macro = repository()->macro(reference.name);
    if (!macro) {
        onDone(make_ret(Ret::Code::NotSupported, qtrc("macromanager", "Macro “%1” not found").arg(name).toStdString()));
        return;
    }

    m_macroStack.push_back(reference.name);
    runSteps(macro->steps, [this, onDone](const Ret& ret) {
        m_macroStack.pop_back();
        onDone(ret);
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
    if (ret) {
        if (m_files.empty()) {
            showCompleted();
        }
    } else {
        undoToStartState();
        showError(ret);
    }

    m_macroStack.clear();
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

    if (!m_files.empty() && importexport::isExportCommand(step.command)) {
        params[importexport::EXPORT_NAME_PARAM] = Val(m_outputNames.at(m_fileIndex));
    }

    return params;
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

void MacroRunner::showCompleted()
{
    const QString name = QString::fromStdString(m_macro.name);
    const QString title = qtrc("macromanager", "“%1” ran successfully").arg(name);
    const QString result = qtrc("macromanager", "%n step(s) applied to the project", nullptr, int(m_appliedStepCount));
    const std::string message = QString("\n%1\n%2\n\n").arg(title, result).toStdString();

    toastService()->showWithTimeout(trc("macromanager", "Macro completed"), message, COMPLETED_TOAST_TIMEOUT,
                                    ui::IconCode::Code::TICK);
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
