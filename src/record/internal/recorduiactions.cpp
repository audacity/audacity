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

static const ActionCode RECORD_START_ACTION_CODE("action://record/start");
static const ActionCode RECORD_PAUSE_ACTION_CODE("action://record/pause");
static const ActionCode RECORD_STOP_ACTION_CODE("action://record/stop");
static const ActionCode RECORD_LEVEL_ACTION_CODE("action://record/level");

const UiActionList RecordUiActions::m_mainActions = {
    UiAction(RECORD_START_ACTION_CODE,
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_FOCUSED,
             TranslatableString("action", "Record"),
             TranslatableString("action", "Record"),
             IconCode::Code::RECORD_FILL
             ),
    UiAction(RECORD_PAUSE_ACTION_CODE,
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_FOCUSED,
             TranslatableString("action", "Pause"),
             TranslatableString("action", "Pause"),
             IconCode::Code::PAUSE_FILL
             ),
    UiAction(RECORD_STOP_ACTION_CODE,
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_OPENED,
             TranslatableString("action", "Stop"),
             TranslatableString("action", "Stop record"),
             IconCode::Code::STOP_FILL
             ),
    UiAction(RECORD_LEVEL_ACTION_CODE,
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_FOCUSED,
             TranslatableString("action", "Record level"),
             TranslatableString("action", "Set record level"),
             IconCode::Code::MICROPHONE
             ),
};

RecordUiActions::RecordUiActions(std::shared_ptr<RecordController> controller)
    : m_controller(controller)
{
}

void RecordUiActions::init()
{
    m_controller->isRecordAllowedChanged().onNotify(this, [this]() {
        ActionCodeList codes;

        for (const UiAction& action : actionsList()) {
            codes.push_back(action.code);
        }

        m_actionEnabledChanged.send(codes);
    });

    m_controller->isRecordingChanged().onNotify(this, [this]() {
        ActionCodeList codes= {
            RECORD_START_ACTION_CODE,
            RECORD_PAUSE_ACTION_CODE,
            RECORD_STOP_ACTION_CODE
        };

        m_actionEnabledChanged.send(codes);
    });
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
