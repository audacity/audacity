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
    UiAction("select-all",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Select all"),
             TranslatableString("action", "Select all")
             ),
    UiAction("clear-selection",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Clear selection"),
             TranslatableString("action", "Clear selection")
             ),
    UiAction("cut",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Cut"),
             TranslatableString("action", "Cut"),
             IconCode::Code::CUT
             ),
    UiAction("copy",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Copy"),
             TranslatableString("action", "Copy"),
             IconCode::Code::COPY
             ),
    UiAction("delete",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "De&lete"),
             TranslatableString("action", "Delete"),
             IconCode::Code::DELETE_TANK
             ),
    UiAction("split-cut",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Cut and leave gap"),
             TranslatableString("action", "Cut and leave gap"),
             IconCode::Code::DELETE_TANK
             ),
    UiAction("split-delete",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Delete and leave gap"),
             TranslatableString("action", "Delete and leave gap"),
             IconCode::Code::DELETE_TANK
             ),
    UiAction("split",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Split"),
             TranslatableString("action", "Split")
             ),
    UiAction("join",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Merge selected clips"),
             TranslatableString("action", "Merge selected clips")
             ),
    UiAction("undo",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "&Undo"),
             TranslatableString("action", "Undo"),
             IconCode::Code::UNDO
             ),
    UiAction("redo",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "&Redo"),
             TranslatableString("action", "Redo"),
             IconCode::Code::REDO
             ),
    UiAction("duplicate",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Duplicate"),
             TranslatableString("action", "Duplicate")
             ),
    // track actions
    UiAction("track-rename",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Rename"),
             TranslatableString("action", "Rename track")
             ),
    UiAction("track-duplicate",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Duplicate"),
             TranslatableString("action", "Duplicate track")
             ),
    UiAction("track-delete",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Delete"),
             TranslatableString("action", "Delete track")
             ),
    UiAction("track-move-up",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Move up"),
             TranslatableString("action", "Move track up")
             ),
    UiAction("track-move-down",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Move down"),
             TranslatableString("action", "Move track down")
             ),
    UiAction("track-move-top",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Move to top"),
             TranslatableString("action", "Move track to top")
             ),
    UiAction("track-move-bottom",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Move to bottom"),
             TranslatableString("action", "Move track to bottom")
             ),
    UiAction("track-make-stereo",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Make stereo"),
             TranslatableString("action", "Make stereo")
             ),
    UiAction("track-swap-stereo",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Swap stereo"),
             TranslatableString("action", "Swap stereo")
             ),
    UiAction("track-split-stereo",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Split stereo"),
             TranslatableString("action", "Split stereo")
             ),
    // clip actions
    UiAction("clip-cut",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Cut"),
             TranslatableString("action", "Cut clip")
             ),
    UiAction("multi-clip-cut",
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
    UiAction("multi-clip-copy",
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
    UiAction("multi-clip-delete",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Delete"),
             TranslatableString("action", "Delete clip")
             ),
    UiAction("clip-cut-selected",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Cut"),
             TranslatableString("action", "Cut"),
             IconCode::Code::CUT
             ),
    UiAction("clip-copy-selected",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Copy"),
             TranslatableString("action", "Copy"),
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
             TranslatableString("action", "Delete")
             ),
    UiAction("track-split",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Split"),
             TranslatableString("action", "Split")
             ),
    UiAction("track-split-at",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Split at"),
             TranslatableString("action", "Split at")
             ),
    UiAction("merge-selected-on-tracks",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Merge selected clips"),
             TranslatableString("action", "Merge selected clips")
             ),
    UiAction("duplicate-selected",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Duplicate selected"),
             TranslatableString("action", "Duplicate selected")
             ),
    UiAction("duplicate-clip",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Duplicate clip"),
             TranslatableString("action", "Duplicate clip")
             ),
    UiAction("clip-split-cut",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Cut and leave gap"),
             TranslatableString("action", "Cut and leave gap")
             ),
    UiAction("clip-split-delete",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Delete and leave gap"),
             TranslatableString("action", "Delete and leave gap")
             ),
    UiAction("split-cut-selected",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Cut and leave gap"),
             TranslatableString("action", "Cut and leave gap")
             ),
    UiAction("split-delete-selected",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Delete and leave gap"),
             TranslatableString("action", "Delete and leave gap")
             ),
    UiAction("clip-export",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Export clip"),
             TranslatableString("action", "Export clip")
             ),
    UiAction("stretch-clip-to-match-tempo",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Stretch with tempo changes"),
             TranslatableString("action", "Stretch with tempo changes"),
             Checkable::Yes
             ),
    UiAction("clip-render-pitch-speed",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Render pitch and speed"),
             TranslatableString("action", "Render pitch and speed")
             ),
    UiAction("new-mono-track",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "New mono track"),
             TranslatableString("action", "New mono track")
             ),
    UiAction("new-stereo-track",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "New stereo track"),
             TranslatableString("action", "New stereo track")
             ),
    UiAction("new-label-track",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "New label track"),
             TranslatableString("action", "New label track")
             ),
    UiAction("trim-audio-outside-selection",
             au::context::UiCtxProjectOpened,
             au::context::CTX_ANY,
             TranslatableString("action", "Trim"),
             TranslatableString("action", "Trim"),
             IconCode::Code::TRIM_AUDIO_OUTSIDE_SELECTION
             ),
    UiAction("silence-audio-selection",
             au::context::UiCtxProjectOpened,
             au::context::CTX_ANY,
             TranslatableString("action", "Silence"),
             TranslatableString("action", "Silence"),
             IconCode::Code::SILENCE_AUDIO_SELECTION
             ),
    UiAction("group-clips",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Group clips"),
             TranslatableString("action", "Group clips")
             ),
    UiAction("ungroup-clips",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Ungroup clips"),
             TranslatableString("action", "Ungroup clips")
             ),
};

TrackeditUiActions::TrackeditUiActions(std::shared_ptr<TrackeditActionsController> controller)
    : m_controller(controller)
{
}

void TrackeditUiActions::init()
{
    m_controller->actionEnabledChanged().onReceive(this, [this](const ActionCode& code) {
        m_actionEnabledChanged.send({ code });
    });

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
