/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QTimer>

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"
#include "framework/global/types/ret.h"
#include "framework/interactive/iinteractive.h"
#include "framework/rcommand/commandable.h"
#include "framework/rcommand/icommanddispatcher.h"
#include "framework/toast/itoastservice.h"

#include "context/iglobalcontext.h"
#include "project/iaudacityproject.h"
#include "project/iprojectfilescontroller.h"
#include "record/irecord.h"
#include "record/irecordcontroller.h"
#include "record/itimedrecordingcontroller.h"
#include "record/timedrecordingtypes.h"

namespace au::record {
class TimedRecordingController : public ITimedRecordingController, public muse::rcommand::Commandable, public muse::async::Asyncable,
    public muse::Contextable
{
    muse::GlobalInject<muse::toast::IToastService> toastService;

    muse::ContextInject<muse::rcommand::ICommandDispatcher> commandDispatcher{ this };
    muse::ContextInject<muse::IInteractive> interactive{ this };
    muse::ContextInject<au::context::IGlobalContext> globalContext{ this };
    muse::ContextInject<IRecordController> recordController{ this };
    muse::ContextInject<IRecord> record{ this };
    muse::ContextInject<project::IProjectFilesController> projectFilesController{ this };

public:
    TimedRecordingController(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    void init();

    State state() const override;
    muse::async::Notification stateChanged() const override;

    std::chrono::system_clock::time_point startTime() const override;
    std::chrono::system_clock::time_point endTime() const override;

    void requestStop() override;

private:
    enum class Outcome {
        Completed,
        Stopped,
        Failed
    };

    muse::Ret openSetupDialog();
    void begin(const TimedRecordingSchedule& schedule);
    void openProgressDialog();
    void onTick();
    void startRecording();
    void failToStart(const std::string& reason);
    void stopRecording(Outcome outcome);
    void confirmStop();
    void finish(Outcome outcome);
    void scheduleCompletionActions();
    void runCompletionActions(const project::IAudacityProjectPtr& project, const muse::io::path_t& saveProjectPath);
    void setState(State state);

    State m_state = State::Idle;
    muse::async::Notification m_stateChanged;

    TimedRecordingSchedule m_schedule;
    muse::io::path_t m_saveProjectPath;
    std::weak_ptr<project::IAudacityProject> m_project;
    bool m_completionActionsPending = false;
    TimedRecordingSchedule::TimePoint m_startTime;
    TimedRecordingSchedule::TimePoint m_endTime;

    bool m_stopConfirmationOpen = false;

    QTimer m_ticker;
};
}
