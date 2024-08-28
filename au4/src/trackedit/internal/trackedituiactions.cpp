/*
* Audacity: A Digital Audio Editor
*/
#include "trackedituiactions.h"

#include "ui/view/iconcodes.h"
#include "context/uicontext.h"
#include "context/shortcutcontext.h"
#include "types/translatablestring.h"

using namespace au::trackedit;
using namespace muse;
using namespace muse::ui;
using namespace muse::actions;

const UiActionList TrackeditUiActions::m_actions = {
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
    UiAction("copy",
             au::context::UiCtxProjectFocused,
             au::context::CTX_PROJECT_FOCUSED,
             TranslatableString("action", "Copy"),
             TranslatableString("action", "Copy"),
             IconCode::Code::COPY
             ),
    UiAction("delete",
             au::context::UiCtxProjectFocused,
             au::context::CTX_PROJECT_FOCUSED,
             TranslatableString("action", "De&lete"),
             TranslatableString("action", "Delete"),
             IconCode::Code::DELETE_TANK
             ),
    // clip
    UiAction("clip-cut",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Cut"),
             TranslatableString("action", "Cut clip")
             ),
    UiAction("clip-copy",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Copy"),
             TranslatableString("action", "Copy clip")
             ),
    UiAction("clip-delete",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Delete"),
             TranslatableString("action", "Delete clip")
             ),
    UiAction("clip-cut-selected",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Cut"),
             TranslatableString("action", "Cut clip"),
             IconCode::Code::CUT
             ),
    UiAction("clip-copy-selected",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Copy"),
             TranslatableString("action", "Copy clip"),
             IconCode::Code::COPY
             ),
    UiAction("paste",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "&Paste"),
             TranslatableString("action", "Paste"),
             IconCode::Code::PASTE
             ),
    UiAction("clip-delete-selected",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Delete"),
             TranslatableString("action", "Delete clip")
             ),
    UiAction("clip-cut-close-gap",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Cut & close gap"),
             TranslatableString("action", "Cut clip & close gap")
             ),
    UiAction("clip-delete-close-gap",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Delete & close gap"),
             TranslatableString("action", "Delete clip & close gap")
             ),
    UiAction("clip-duplicate",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Duplicate clip"),
             TranslatableString("action", "Duplicate clip")
             ),
    UiAction("clip-split",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Split clip"),
             TranslatableString("action", "Split clip")
             ),
    UiAction("clip-export",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Export clip"),
             TranslatableString("action", "Export clip")
             ),
    UiAction("clip-enable-stretching",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Enable clip stretching"),
             TranslatableString("action", "Enable clip stretching")
             ),
    //! NOTE It is not clear yet what this is, if it is a dialog show,
    //! then it might be better to move it to the projectscene module
    UiAction("clip-pitch-speed",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Pitch and speed"),
             TranslatableString("action", "Pitch and speed")
             ),
    UiAction("clip-render-pitch-speed",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Render pitch and speed"),
             TranslatableString("action", "Render pitch and speed")
             ),
    //! ----------
};

TrackeditUiActions::TrackeditUiActions(std::shared_ptr<TrackeditActionsController> controller)
    : m_controller(controller)
{
}

void TrackeditUiActions::init()
{
    m_controller->actionCheckedChanged().onReceive(this, [this](const ActionCode& code) {
        m_actionCheckedChanged.send({ code });
    });
}

const UiActionList& TrackeditUiActions::actionsList() const
{
    return m_actions;
}

bool TrackeditUiActions::actionEnabled(const UiAction& act) const
{
    if (!m_controller->canReceiveAction(act.code)) {
        return false;
    }

    return true;
}

bool TrackeditUiActions::actionChecked(const UiAction& act) const
{
    return m_controller->actionChecked(act.code);
}

muse::async::Channel<ActionCodeList> TrackeditUiActions::actionEnabledChanged() const
{
    return m_actionEnabledChanged;
}

muse::async::Channel<ActionCodeList> TrackeditUiActions::actionCheckedChanged() const
{
    return m_actionCheckedChanged;
}
