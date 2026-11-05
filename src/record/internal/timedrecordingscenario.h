/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QTimer>
#include <QVariantMap>

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"
#include "framework/global/types/ret.h"
#include "framework/interactive/iinteractive.h"
#include "framework/rcommand/icommanddispatcher.h"
#include "framework/toast/itoastservice.h"

#include "context/iglobalcontext.h"
#include "importexport/export/iexportconfiguration.h"
#include "importexport/export/iexporter.h"
#include "project/iaudacityproject.h"
#include "project/iprojectfilescontroller.h"
#include "record/irecord.h"
#include "record/irecordconfiguration.h"
#include "record/irecordcontroller.h"
#include "record/itimedrecordingscenario.h"
#include "record/timedrecordingtypes.h"

namespace au::record {
class TimedRecordingScenario : public ITimedRecordingScenario, public muse::async::Asyncable, public muse::Contextable
{
    muse::GlobalInject<muse::toast::IToastService> toastService;
    muse::GlobalInject<importexport::IExportConfiguration> exportConfiguration;
    muse::GlobalInject<IRecordConfiguration> recordConfiguration;

    muse::ContextInject<muse::rcommand::ICommandDispatcher> commandDispatcher{ this };
    muse::ContextInject<muse::IInteractive> interactive{ this };
    muse::ContextInject<au::context::IGlobalContext> globalContext{ this };
    muse::ContextInject<IRecordController> recordController{ this };
    muse::ContextInject<IRecord> record{ this };
    muse::ContextInject<project::IProjectFilesController> projectFilesController{ this };
    muse::ContextInject<importexport::IExporter> exporter{ this };

public:
    TimedRecordingScenario(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    void init();

    muse::Ret openSetup() override;

    State state() const override;
    muse::async::Notification stateChanged() const override;

    std::chrono::system_clock::time_point startTime() const override;
    std::chrono::system_clock::time_point endTime() const override;
    std::chrono::system_clock::time_point quitTime() const override;

    void requestStop() override;

private:
    struct CompletionActions {
        muse::io::path_t saveProjectPath;
        muse::io::path_t exportAudioPath;
        std::string exportFormat;
        bool quitAudacity = false;
    };

    enum class Outcome {
        Completed,
        Stopped,
        Failed
    };

    void onSetupAccepted(const QVariantMap& data);
    void begin(const TimedRecordingSchedule& schedule);
    void openProgressDialog();
    void onTick();
    void startRecording();
    void failToStart(const std::string& reason);
    void stopRecording(Outcome outcome);
    void confirmStop();
    void finish(Outcome outcome);
    void scheduleCompletionActions();
    void runCompletionActions(const project::IAudacityProjectPtr& project, const CompletionActions& actions);
    bool exportAudio(const project::IAudacityProjectPtr& project, const muse::io::path_t& path, const std::string& format);
    void beginQuitCountdown();
    void quitAudacity();
    void cancelQuit();
    void setState(State state);

    State m_state = State::Idle;
    muse::async::Notification m_stateChanged;

    TimedRecordingSchedule m_schedule;
    CompletionActions m_completionActions;
    std::weak_ptr<project::IAudacityProject> m_project;
    bool m_completionActionsPending = false;
    TimedRecordingSchedule::TimePoint m_startTime;
    TimedRecordingSchedule::TimePoint m_endTime;
    TimedRecordingSchedule::TimePoint m_quitTime;

    bool m_stopConfirmationOpen = false;

    QTimer m_ticker;
};
}
