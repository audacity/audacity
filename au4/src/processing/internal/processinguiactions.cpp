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
    UiAction("toggle-loop-region",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Toggle loop region"),
             TranslatableString("action", "Toggle loop region")
             ),
    UiAction("clear-loop-region",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Clear loop region"),
             TranslatableString("action", "Clear loop region")
             ),
    UiAction("set-loop-region-to-selection",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Set loop region to selection"),
             TranslatableString("action", "Set loop region to selection")
             ),
    UiAction("set-selection-to-loop",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Set selection to loop"),
             TranslatableString("action", "Set selection to loop")
             ),
    UiAction("set-loop-region-in",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Set loop region in"),
             TranslatableString("action", "Set loop region in")
             ),
    UiAction("set-loop-region-out",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Set loop region out"),
             TranslatableString("action", "Set loop region out")
             ),
};

ProcessingUiActions::ProcessingUiActions(std::shared_ptr<ProcessingActionsController> controller)
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
