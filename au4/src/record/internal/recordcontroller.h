/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_RECORD_RECORDCONTROLLER_H
#define AU_RECORD_RECORDCONTROLLER_H

#include "modularity/ioc.h"
#include "async/asyncable.h"
#include "actions/iactionsdispatcher.h"
#include "actions/actionable.h"
#include "context/iglobalcontext.h"
#include "iinteractive.h"

#include "au3wrap/iau3record.h"

#include "../irecordcontroller.h"

namespace au::record {
class RecordController : public IRecordController, public muse::actions::Actionable, public muse::async::Asyncable
{
    INJECT_STATIC(muse::actions::IActionsDispatcher, dispatcher)
    INJECT_STATIC(au::context::IGlobalContext, globalContext)
    INJECT_STATIC(muse::IInteractive, interactive)
    INJECT_STATIC(au3::IAu3Record, au3Record)

public:
    void init();
    void deinit();

    bool isRecording() const override;
    muse::async::Notification isRecordingChanged() const override;

    bool canReceiveAction(const muse::actions::ActionCode& code) const override;

private:
    enum class RecordStatus {
        Stopped = 0,
        Running
    };

    void toggleRecord();
    void start();
    void stop();

    void setCurrentRecordStatus(RecordStatus status);

    muse::async::Notification m_isRecordingChanged;

    RecordStatus m_currentPlaybackStatus = RecordStatus::Stopped;
};
}

#endif // AU_RECORD_RECORDCONTROLLER_H
