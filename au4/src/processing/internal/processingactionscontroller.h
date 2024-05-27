/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_PROCESSING_PROCESSINGACTIONSCONTROLLER_H
#define AU_PROCESSING_PROCESSINGACTIONSCONTROLLER_H

#include "modularity/ioc.h"
#include "async/asyncable.h"
#include "actions/iactionsdispatcher.h"
#include "actions/actionable.h"
#include "context/iglobalcontext.h"
#include "iinteractive.h"

#include "../iprocessingactionscontroller.h"

namespace au::processing {
class ProcessingActionsController : public IProcessingActionsController, public muse::actions::Actionable, public muse::async::Asyncable
{
    INJECT_STATIC(muse::actions::IActionsDispatcher, dispatcher)
    INJECT_STATIC(au::context::IGlobalContext, globalContext)
    INJECT_STATIC(muse::IInteractive, interactive)

public:
    void init();

    bool actionChecked(const muse::actions::ActionCode& actionCode) const override;
    muse::async::Channel<muse::actions::ActionCode> actionCheckedChanged() const override;
    bool canReceiveAction(const muse::actions::ActionCode& code) const override;

private:
    void notifyActionCheckedChanged(const muse::actions::ActionCode& actionCode);

    void toggleLoopRegion();
    void clearLoopRegion();
    void setLoopRegionToSelection();
    void setSelectionToLoop();
    void setLoopRegionIn();
    void setLoopRegionOut();

    muse::async::Channel<muse::actions::ActionCode> m_actionCheckedChanged;
};
}

#endif // AU_PROCESSING_PROCESSINGACTIONSCONTROLLER_H
