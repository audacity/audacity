/*
* Audacity: A Digital Audio Editor
*/
#include "recorduiactions.h"

#include "framework/ui/view/iconcodes.h"
#include "framework/global/types/translatablestring.h"

#include "context/uicontext.h"
#include "context/shortcutcontext.h"

using namespace au::record;
using namespace muse;
using namespace muse::ui;
using namespace muse::actions;

static const ActionQuery RECORD_START_QUERY("action://record/start");
static const ActionQuery RECORD_PAUSE_QUERY("action://record/pause");
static const ActionQuery RECORD_STOP_QUERY("action://record/stop");
static const ActionQuery RECORD_LEVEL_QUERY("action://record/level");
static const ActionQuery RECORD_TOGGLE_MIC_METERING("action://record/toggle-mic-metering");
static const ActionQuery RECORD_TOGGLE_INPUT_MONITORING("action://record/toggle-input-monitoring");

const UiActionList RecordUiActions::m_mainActions = {
    UiAction(RECORD_START_QUERY.toString(),
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_FOCUSED,
             TranslatableString("action", "Record"),
             TranslatableString("action", "Record"),
             IconCode::Code::RECORD_FILL
             ),
    UiAction(RECORD_PAUSE_QUERY.toString(),
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_FOCUSED,
             TranslatableString("action", "Pause"),
             TranslatableString("action", "Pause"),
             IconCode::Code::PAUSE_FILL
             ),
    UiAction(RECORD_STOP_QUERY.toString(),
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_OPENED,
             TranslatableString("action", "Stop"),
             TranslatableString("action", "Stop record"),
             IconCode::Code::STOP_FILL
             ),
    UiAction(RECORD_LEVEL_QUERY.toString(),
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_FOCUSED,
             TranslatableString("action", "Record level"),
             TranslatableString("action", "Set record level"),
             IconCode::Code::MICROPHONE
             ),
    UiAction(RECORD_TOGGLE_MIC_METERING.toString(),
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Show mic metering"),
             TranslatableString("action", "Show mic metering"),
             Checkable::Yes
             ),
    UiAction(RECORD_TOGGLE_INPUT_MONITORING.toString(),
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Turn on input monitoring"),
             TranslatableString("action", "Turn on input monitoring"),
             Checkable::Yes
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
        ActionCodeList codes;

        for (const UiAction& action : actionsList()) {
            codes.push_back(action.code);
        }

        m_actionEnabledChanged.send(codes);
    });

    m_controller->isMicMeteringOnChanged().onNotify(this, [this]() {
        m_actionCheckedChanged.send(ActionCodeList { RECORD_TOGGLE_MIC_METERING.toString() });
    });

    m_controller->isInputMonitoringOnChanged().onNotify(this, [this]() {
        m_actionCheckedChanged.send(ActionCodeList { RECORD_TOGGLE_INPUT_MONITORING.toString() });
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
    return m_controller->canReceiveAction(act.code);
}

bool RecordUiActions::actionChecked(const UiAction& act) const
{
    if (act.code == RECORD_TOGGLE_MIC_METERING.toString()) {
        return m_controller->isMicMeteringOn();
    }

    if (act.code == RECORD_TOGGLE_INPUT_MONITORING.toString()) {
        return m_controller->isInputMonitoringOn();
    }

    return false;
}

muse::async::Channel<ActionCodeList> RecordUiActions::actionEnabledChanged() const
{
    return m_actionEnabledChanged;
}

muse::async::Channel<ActionCodeList> RecordUiActions::actionCheckedChanged() const
{
    return m_actionCheckedChanged;
}
