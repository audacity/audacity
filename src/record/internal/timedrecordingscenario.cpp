/*
* Audacity: A Digital Audio Editor
*/
#include "timedrecordingscenario.h"

#include <QVariantMap>

#include "framework/global/async/async.h"
#include "framework/global/translation.h"
#include "framework/ui/view/iconcodes.h"

#include "../recordcommands.h"
#include "timedrecordingformatter.h"

using namespace au::record;
using namespace muse;

namespace {
const UriQuery SETUP_DIALOG_URI("audacity://record/timedrecording");
const UriQuery PROGRESS_DIALOG_URI("audacity://record/timedrecording/progress");

constexpr int TICK_INTERVAL_MS = 100;
constexpr std::chrono::seconds QUIT_COUNTDOWN(60);

using TimePoint = TimedRecordingSchedule::TimePoint;

TimePoint now()
{
    return std::chrono::system_clock::now();
}

TimedRecordingSchedule scheduleFromDialogData(const QVariantMap& data)
{
    TimedRecordingSchedule schedule;
    if (data.contains("startMs")) {
        schedule.start = TimePoint { std::chrono::milliseconds { data.value("startMs").toLongLong() } };
    }
    if (data.contains("endMs")) {
        schedule.end = TimePoint { std::chrono::milliseconds { data.value("endMs").toLongLong() } };
    } else {
        schedule.end = TimedRecordingSchedule::Duration(data.value("durationSeconds").toLongLong());
    }
    return schedule;
}
}

void TimedRecordingScenario::init()
{
    m_ticker.setInterval(TICK_INTERVAL_MS);
    QObject::connect(&m_ticker, &QTimer::timeout, &m_ticker, [this]() { onTick(); });

    recordController()->isRecordingChanged().onNotify(this, [this]() {
        if (m_state == State::Recording && !recordController()->isRecording()) {
            finish(Outcome::Stopped);
        }
    });

    record()->recordingFinished().onNotify(this, [this]() {
        if (m_completionActionsPending) {
            m_completionActionsPending = false;
            scheduleCompletionActions();
        }
    });

    globalContext()->currentProjectChanged().onNotify(this, [this]() {
        if (m_state == State::Quitting) {
            cancelQuit();
        } else if (m_state != State::Idle) {
            stopRecording(Outcome::Stopped);
        }
    });
}

ITimedRecordingScenario::State TimedRecordingScenario::state() const
{
    return m_state;
}

async::Notification TimedRecordingScenario::stateChanged() const
{
    return m_stateChanged;
}

std::chrono::system_clock::time_point TimedRecordingScenario::startTime() const
{
    return m_startTime;
}

std::chrono::system_clock::time_point TimedRecordingScenario::endTime() const
{
    return m_endTime;
}

std::chrono::system_clock::time_point TimedRecordingScenario::quitTime() const
{
    return m_quitTime;
}

void TimedRecordingScenario::requestStop()
{
    switch (m_state) {
    case State::Idle:
        return;
    case State::Waiting:
        finish(Outcome::Stopped);
        return;
    case State::Recording:
        confirmStop();
        return;
    case State::Quitting:
        cancelQuit();
        return;
    }
}

Ret TimedRecordingScenario::openSetup()
{
    if (m_state != State::Idle || interactive()->isOpened(SETUP_DIALOG_URI).val) {
        return make_ret(Ret::Code::Cancel);
    }

    interactive()->open(SETUP_DIALOG_URI).onResolve(this, [this](const Val& val) {
        onSetupAccepted(val.toQVariant().toMap());
    });

    return make_ok();
}

void TimedRecordingScenario::onSetupAccepted(const QVariantMap& data)
{
    if (m_state != State::Idle) {
        return;
    }

    m_completionActions.saveProjectPath = data.value("saveProjectPath").toString();
    m_completionActions.exportAudioPath = data.value("exportAudioPath").toString();
    m_completionActions.exportFormat = data.value("exportFormat").toString().toStdString();
    m_completionActions.quitAudacity = data.value("quitAudacity").toBool();

    TimedRecordingOptions options;
    options.durationSeconds = data.value("durationSeconds").toLongLong();
    options.saveProject = !m_completionActions.saveProjectPath.empty();
    options.exportAudio = !m_completionActions.exportAudioPath.empty();
    options.quitAudacity = m_completionActions.quitAudacity;
    recordConfiguration()->setTimedRecordingOptions(options);

    if (!m_completionActions.exportAudioPath.empty()) {
        exportConfiguration()->setDirectoryPath(io::dirpath(m_completionActions.exportAudioPath));
        exportConfiguration()->setCurrentFormat(m_completionActions.exportFormat);
    }

    begin(scheduleFromDialogData(data));
}

void TimedRecordingScenario::begin(const TimedRecordingSchedule& schedule)
{
    const TimePoint startedAt = now();
    if (!schedule.isValid(startedAt)) {
        failToStart(muse::trc("record", "The end time has already passed."));
        return;
    }

    m_schedule = schedule;
    m_completionActionsPending = false;
    m_project = globalContext()->currentProject();
    m_startTime = schedule.startTime(startedAt);
    m_endTime = schedule.endTime(startedAt);

    setState(State::Waiting);
    openProgressDialog();
    m_ticker.start();
    onTick();
}

void TimedRecordingScenario::openProgressDialog()
{
    interactive()->open(PROGRESS_DIALOG_URI)
    .onResolve(this, [this](const Val&) { requestStop(); })
    .onReject(this, [this](int, const std::string&) { requestStop(); });
}

void TimedRecordingScenario::onTick()
{
    const TimePoint tickedAt = now();

    if (m_state == State::Waiting && tickedAt >= m_startTime) {
        startRecording();
    } else if (m_state == State::Recording && tickedAt >= m_endTime) {
        stopRecording(Outcome::Completed);
    } else if (m_state == State::Quitting && tickedAt >= m_quitTime) {
        quitAudacity();
    }
}

void TimedRecordingScenario::startRecording()
{
    if (recordController()->isRecording()) {
        failToStart(muse::trc("record", "Audacity was already recording."));
        return;
    }

    commandDispatcher()->dispatch(RECORD_START_COMMAND).onResolve(this, [this](const rcommand::Response& response) {
        if (m_state != State::Waiting) {
            return;
        }

        if (!response.ret.success() || !recordController()->isRecording()) {
            failToStart(response.ret.text().empty() ? muse::trc("record", "Recording couldn't be started.") : response.ret.text());
            return;
        }

        const TimePoint startedAt = now();
        m_startTime = startedAt;
        m_endTime = m_schedule.endTime(startedAt);
        setState(State::Recording);
    });
}

void TimedRecordingScenario::failToStart(const std::string& reason)
{
    finish(Outcome::Failed);
    toastService()->showError(muse::trc("record", "Timed recording couldn't start"), reason);
}

void TimedRecordingScenario::stopRecording(Outcome outcome)
{
    if (m_state == State::Recording && recordController()->isRecording()) {
        finish(outcome);
        commandDispatcher()->dispatch(RECORD_STOP_COMMAND);
        return;
    }

    finish(outcome);
}

void TimedRecordingScenario::confirmStop()
{
    if (m_stopConfirmationOpen) {
        return;
    }
    m_stopConfirmationOpen = true;

    constexpr int confirmButton = int(IInteractive::Button::CustomButton) + 1;
    const IInteractive::ButtonDatas buttons {
        IInteractive::ButtonData(IInteractive::Button::Cancel, muse::trc("global", "Cancel")),
        IInteractive::ButtonData(confirmButton, muse::trc("record", "Confirm"), true)
    };

    const std::string title = muse::trc("record", "Stop timed recording");
    interactive()->warning(title, muse::trc("record", "Your timed recording will be stopped."), buttons, confirmButton,
                           { IInteractive::Option::WithIcon }, title)
    .onResolve(this, [this, confirmButton](const IInteractive::Result& result) {
        m_stopConfirmationOpen = false;

        if (m_state != State::Recording) {
            return;
        }

        if (result.isButton(confirmButton)) {
            stopRecording(Outcome::Stopped);
            return;
        }

        if (!interactive()->isOpened(PROGRESS_DIALOG_URI).val) {
            openProgressDialog();
        }
    });
}

void TimedRecordingScenario::finish(Outcome outcome)
{
    if (m_state == State::Idle) {
        return;
    }

    const bool wasRecording = m_state == State::Recording;
    const auto recorded
        = wasRecording ? std::chrono::duration_cast<std::chrono::seconds>(now() - m_startTime) : std::chrono::seconds::zero();

    m_ticker.stop();
    setState(State::Idle);

    interactive()->close(PROGRESS_DIALOG_URI);

    switch (outcome) {
    case Outcome::Completed:
        toastService()->show(muse::trc("record", "Timed recording complete"),
                             //: %1 is how long the timed recording lasted, e.g. "1h 30m"
                             muse::mtrc("record", "Recorded for %1").arg(TimedRecordingFormatter::duration(recorded)).toStdString(),
                             ui::IconCode::Code::TICK);
        break;
    case Outcome::Stopped:
        toastService()->show(muse::trc("record", "Timed recording stopped"), "", ui::IconCode::Code::TICK);
        break;
    case Outcome::Failed:
        break;
    }

    if (!wasRecording || outcome != Outcome::Completed) {
        return;
    }

    if (recordController()->isRecording()) {
        m_completionActionsPending = true;
        return;
    }

    scheduleCompletionActions();
}

void TimedRecordingScenario::scheduleCompletionActions()
{
    async::Async::call(this, [this, project = m_project, actions = m_completionActions]() {
        runCompletionActions(project.lock(), actions);
    });
}

void TimedRecordingScenario::runCompletionActions(const project::IAudacityProjectPtr& project, const CompletionActions& actions)
{
    if (!project || project != globalContext()->currentProject()) {
        return;
    }

    bool succeeded = true;

    if (!actions.saveProjectPath.empty()) {
        if (projectFilesController()->saveProject(actions.saveProjectPath)) {
            toastService()->show(muse::trc("record", "Project saved"), actions.saveProjectPath.toStdString(), ui::IconCode::Code::SAVE);
        } else {
            toastService()->showError(muse::trc("record", "Couldn't save the project"), actions.saveProjectPath.toStdString());
            succeeded = false;
        }
    }

    if (!actions.exportAudioPath.empty() && !exportAudio(project, actions.exportAudioPath, actions.exportFormat)) {
        succeeded = false;
    }

    if (actions.quitAudacity && succeeded) {
        beginQuitCountdown();
    }
}

bool TimedRecordingScenario::exportAudio(const project::IAudacityProjectPtr& project, const muse::io::path_t& path,
                                         const std::string& format)
{
    importexport::IExporter::Options options;
    options[importexport::IExporter::OptionKey::Format] = Val(format);
    options[importexport::IExporter::OptionKey::ProcessType] = Val(importexport::ExportProcessType::FULL_PROJECT_AUDIO);

    const Ret ret = exporter()->exportData(path, options, nullptr, project);
    if (!ret) {
        toastService()->showError(muse::trc("record", "Couldn't export the audio"), ret.text().empty() ? path.toStdString() : ret.text());
        return false;
    }

    toastService()->show(muse::trc("record", "Audio exported"), path.toStdString(), ui::IconCode::Code::SHARE_FILE);
    return true;
}

void TimedRecordingScenario::beginQuitCountdown()
{
    if (m_state != State::Idle) {
        return;
    }

    m_quitTime = now() + QUIT_COUNTDOWN;

    setState(State::Quitting);
    openProgressDialog();
    m_ticker.start();
}

void TimedRecordingScenario::quitAudacity()
{
    m_ticker.stop();
    setState(State::Idle);

    interactive()->close(PROGRESS_DIALOG_URI).onResolve(this, [this](const Ret&) {
        projectFilesController()->quitDiscardingUnsavedChanges();
    });
}

void TimedRecordingScenario::cancelQuit()
{
    m_ticker.stop();
    setState(State::Idle);
    interactive()->close(PROGRESS_DIALOG_URI);
}

void TimedRecordingScenario::setState(State state)
{
    if (m_state == state) {
        return;
    }
    m_state = state;
    m_stateChanged.notify();
}
