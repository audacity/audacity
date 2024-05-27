/*
* Audacity: A Digital Audio Editor
*/
#include "projectsceneuiactions.h"

#include "ui/view/iconcodes.h"
#include "context/uicontext.h"
#include "context/shortcutcontext.h"
#include "types/translatablestring.h"

#include "log.h"

using namespace au::projectscene;
using namespace muse;
using namespace muse::ui;
using namespace muse::actions;

const UiActionList ProjectSceneUiActions::m_actions = {
    UiAction("envelope",
             au::context::UiCtxNotationOpened,
             au::context::CTX_NOTATION_OPENED,
             TranslatableString("action", "Evelope tool"),
             TranslatableString("action", "Evelope tool"),
             IconCode::Code::ENVELOPE
             ),
    UiAction("zoom",
             au::context::UiCtxNotationOpened,
             au::context::CTX_ANY,
             TranslatableString("action", "Zoom"),
             TranslatableString("action", "Zoom"),
             IconCode::Code::ZOOM_TOGGLE
             ),
    UiAction("zoomin",
             au::context::UiCtxNotationOpened,
             au::context::CTX_ANY,
             TranslatableString("action", "Zoom in"),
             TranslatableString("action", "Zoom in"),
             IconCode::Code::ZOOM_IN
             ),
    UiAction("zoomout",
             au::context::UiCtxNotationOpened,
             au::context::CTX_ANY,
             TranslatableString("action", "Zoom out"),
             TranslatableString("action", "Zoom out"),
             IconCode::Code::ZOOM_OUT
             ),
    UiAction("fit-selection",
             au::context::UiCtxNotationOpened,
             au::context::CTX_ANY,
             TranslatableString("action", "Fit selection to width"),
             TranslatableString("action", "Fit selection to width"),
             IconCode::Code::FIT_SELECTION
             ),
    UiAction("fit-project",
             au::context::UiCtxNotationOpened,
             au::context::CTX_ANY,
             TranslatableString("action", "Fit project to width"),
             TranslatableString("action", "Fit project to width"),
             IconCode::Code::FIT_PROJECT
             ),
    UiAction("trim-audio-outside-selection",
             au::context::UiCtxNotationOpened,
             au::context::CTX_ANY,
             TranslatableString("action", "Trim audio outside selection"),
             TranslatableString("action", "Trim audio outside selection"),
             IconCode::Code::TRIM_AUDIO_OUTSIDE_SELECTION
             ),
    UiAction("silence-audio-selection",
             au::context::UiCtxNotationOpened,
             au::context::CTX_ANY,
             TranslatableString("action", "Silence audio selection"),
             TranslatableString("action", "Silence audio selection"),
             IconCode::Code::SILENCE_AUDIO_SELECTION
             ),
    UiAction("minutes-seconds-ruler",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Minutes && seconds"),
             TranslatableString("action", "Minutes && seconds"),
             Checkable::Yes
             ),
    UiAction("beats-measures-ruler",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Beats && measures"),
             TranslatableString("action", "Beats && measures"),
             Checkable::Yes
             ),
    UiAction("toggle-vertical-rulers",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Show vertical rulers"),
             TranslatableString("action", "Show vertical rulers"),
             Checkable::Yes
             ),
    UiAction("update-display-while-playing",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Update display while playing"),
             TranslatableString("action", "Update display while playing"),
             Checkable::Yes
             ),
    UiAction("pinned-play-head",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Pinned play head"),
             TranslatableString("action", "Pinned play head"),
             Checkable::Yes
             ),
};

ProjectSceneUiActions::ProjectSceneUiActions(std::shared_ptr<ProjectSceneActionsController> controller)
    : m_controller(controller)
{
}

const UiActionList& ProjectSceneUiActions::actionsList() const
{
    return m_actions;
}

bool ProjectSceneUiActions::actionEnabled(const muse::ui::UiAction& act) const
{
    if (!m_controller->canReceiveAction(act.code)) {
        return false;
    }

    return true;
}

muse::async::Channel<ActionCodeList> ProjectSceneUiActions::actionEnabledChanged() const
{
    static async::Channel<ActionCodeList> ch;
    return ch;
}

bool ProjectSceneUiActions::actionChecked(const muse::ui::UiAction& act) const
{
    return m_controller->actionChecked(act.code);
}

muse::async::Channel<ActionCodeList> ProjectSceneUiActions::actionCheckedChanged() const
{
    static async::Channel<ActionCodeList> ch;
    return ch;
}
