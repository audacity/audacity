/*
* Audacity: A Digital Audio Editor
*/
#include "recorduiactions.h"

#include "ui/view/iconcodes.h"
#include "context/uicontext.h"
#include "context/shortcutcontext.h"
#include "types/translatablestring.h"

using namespace au::record;
using namespace muse;
using namespace muse::ui;
using namespace muse::actions;

const UiActionList RecordUiActions::m_mainActions = {
    UiAction("record",
             au::context::UiCtxNotationOpened,
             au::context::CTX_NOTATION_FOCUSED,
             TranslatableString("action", "Record"),
             TranslatableString("action", "Record"),
             IconCode::Code::RECORD_FILL
             ),
    UiAction("record-level",
             au::context::UiCtxNotationOpened,
             au::context::CTX_NOTATION_FOCUSED,
             TranslatableString("action", "Record level"),
             TranslatableString("action", "Set record level"),
             IconCode::Code::MICROPHONE
             ),
};

RecordUiActions::RecordUiActions(std::shared_ptr<RecordController> controller)
    : m_controller(controller)
{
}

const UiActionList& RecordUiActions::actionsList() const
{
    static UiActionList alist;
    if (alist.empty()) {
        alist.insert(alist.end(), m_mainActions.cbegin(), m_mainActions.cend());
    }
    return alist;
}

bool RecordUiActions::actionEnabled(const UiAction& act) const
{
    if (!m_controller->canReceiveAction(act.code)) {
        return false;
    }

    return true;
}

bool RecordUiActions::actionChecked(const UiAction& act) const
{
    UNUSED(act);
    return false;
}

muse::async::Channel<ActionCodeList> RecordUiActions::actionEnabledChanged() const
{
    return m_actionEnabledChanged;
}

muse::async::Channel<ActionCodeList> RecordUiActions::actionCheckedChanged() const
{
    static async::Channel<muse::actions::ActionCodeList> ch;
    return ch;
}
