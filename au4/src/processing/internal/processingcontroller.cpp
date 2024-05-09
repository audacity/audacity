/*
* Audacity: A Digital Audio Editor
*/
#include "processingcontroller.h"

using namespace muse;
using namespace au::processing;
using namespace muse::async;
using namespace muse::actions;

void ProcessingController::init()
{
}

void ProcessingController::notifyActionCheckedChanged(const ActionCode& actionCode)
{
    m_actionCheckedChanged.send(actionCode);
}

bool ProcessingController::actionChecked(const ActionCode& actionCode) const
{
    //! TODO AU4
    return false;
}

Channel<ActionCode> ProcessingController::actionCheckedChanged() const
{
    return m_actionCheckedChanged;
}

bool ProcessingController::canReceiveAction(const ActionCode&) const
{
    return globalContext()->currentProject() != nullptr;
}
