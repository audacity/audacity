/*
* Audacity: A Digital Audio Editor
*/
#include "processinguiactions.h"

#include "ui/view/iconcodes.h"
#include "context/uicontext.h"
#include "context/shortcutcontext.h"
#include "types/translatablestring.h"

using namespace au::processing;
using namespace muse;
using namespace muse::ui;
using namespace muse::actions;

const UiActionList ProcessingUiActions::m_actions = {
};

ProcessingUiActions::ProcessingUiActions(std::shared_ptr<ProcessingController> controller)
    : m_controller(controller)
{
}

void ProcessingUiActions::init()
{
    m_controller->actionCheckedChanged().onReceive(this, [this](const ActionCode& code) {
        m_actionCheckedChanged.send({ code });
    });
}

const UiActionList& ProcessingUiActions::actionsList() const
{
    return m_actions;
}

bool ProcessingUiActions::actionEnabled(const UiAction& act) const
{
    if (!m_controller->canReceiveAction(act.code)) {
        return false;
    }

    return true;
}

bool ProcessingUiActions::actionChecked(const UiAction& act) const
{
    return m_controller->actionChecked(act.code);
}

muse::async::Channel<ActionCodeList> ProcessingUiActions::actionEnabledChanged() const
{
    return m_actionEnabledChanged;
}

muse::async::Channel<ActionCodeList> ProcessingUiActions::actionCheckedChanged() const
{
    return m_actionCheckedChanged;
}
