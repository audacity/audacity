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
    UiAction("automation",
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_OPENED,
             TranslatableString("action", "Automation"),
             TranslatableString("action", "Automation"),
             IconCode::Code::AUTOMATION
             ),
    UiAction("zoom",
             au::context::UiCtxProjectOpened,
             au::context::CTX_ANY,
             TranslatableString("action", "Zoom toggle"),
             TranslatableString("action", "Zoom toggle"),
             IconCode::Code::ZOOM_TOGGLE
             ),
    UiAction("zoom-in",
             au::context::UiCtxProjectOpened,
             au::context::CTX_ANY,
             TranslatableString("action", "Zoom in"),
             TranslatableString("action", "Zoom in"),
             IconCode::Code::ZOOM_IN
             ),
    UiAction("zoom-out",
             au::context::UiCtxProjectOpened,
             au::context::CTX_ANY,
             TranslatableString("action", "Zoom out"),
             TranslatableString("action", "Zoom out"),
             IconCode::Code::ZOOM_OUT
             ),
    UiAction("fit-selection",
             au::context::UiCtxProjectOpened,
             au::context::CTX_ANY,
             TranslatableString("action", "Fit selection to width"),
             TranslatableString("action", "Fit selection to width"),
             IconCode::Code::FIT_SELECTION
             ),
    UiAction("fit-project",
             au::context::UiCtxProjectOpened,
             au::context::CTX_ANY,
             TranslatableString("action", "Fit project to width"),
             TranslatableString("action", "Fit project to width"),
             IconCode::Code::FIT_PROJECT
             ),
    UiAction("spectral-editing",
             au::context::UiCtxProjectOpened,
             au::context::CTX_ANY,
             TranslatableString("action", "Spectral editing"),
             TranslatableString("action", "Spectral editing"),
             IconCode::Code::SPECTROGRAM
             ),
    UiAction("spectral-box-select",
             au::context::UiCtxProjectOpened,
             au::context::CTX_ANY,
             TranslatableString("action", "Spectral box select"),
             TranslatableString("action", "Spectral box select"),
             IconCode::Code::SPECTROGRAM_BOX_SELECTION
             ),
    UiAction("spectral-brush",
             au::context::UiCtxProjectOpened,
             au::context::CTX_ANY,
             TranslatableString("action", "Spectral box select"),
             TranslatableString("action", "Spectral box select"),
             IconCode::Code::BRUSH
             ),
    UiAction("snap",
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_FOCUSED,
             TranslatableString("action", "Snapping"),
             TranslatableString("action", "Snapping"),
             IconCode::Code::MAGNET
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
    UiAction("show-master-track",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Show master track"),
             TranslatableString("action", "Show master track"),
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
    UiAction("insert-silence",
             au::context::UiCtxProjectOpened,
             muse::shortcuts::CTX_PROJECT_OPENED,
             TranslatableString("action", "Insert Silence"),
             TranslatableString("action", "Insert Silence")
             ),
    // clip
    UiAction("clip-properties",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Clip properties"),
             TranslatableString("action", "Show clip properties")
             ),
    UiAction("clip-rename",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Rename clip"),
             TranslatableString("action", "Rename clip")
             ),
    UiAction("play-position-decrease",
             au::context::UiCtxProjectOpened,
             muse::shortcuts::CTX_PROJECT_OPENED,
             TranslatableString("action", "Move play cursor left"),
             TranslatableString("action", "Move play cursor left")
             ),
    UiAction("play-position-increase",
             au::context::UiCtxProjectOpened,
             muse::shortcuts::CTX_PROJECT_OPENED,
             TranslatableString("action", "Move play cursor right"),
             TranslatableString("action", "Move play cursor right")
             ),
    UiAction("clip-pitch-speed",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Pitch and speed"),
             TranslatableString("action", "Pitch and speed")
             )
};

ProjectSceneUiActions::ProjectSceneUiActions(std::shared_ptr<ProjectSceneActionsController> controller)
    : m_controller(controller)
{
}

void ProjectSceneUiActions::init()
{
    m_controller->actionCheckedChanged().onReceive(this, [this](const ActionCode& code) {
        m_actionCheckedChanged.send({ code });
    });
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
    return m_actionEnabledChanged;
}

bool ProjectSceneUiActions::actionChecked(const muse::ui::UiAction& act) const
{
    return m_controller->actionChecked(act.code);
}

muse::async::Channel<ActionCodeList> ProjectSceneUiActions::actionCheckedChanged() const
{
    return m_actionCheckedChanged;
}

const ToolConfig& ProjectSceneUiActions::defaultPlaybackToolBarConfig()
{
    static ToolConfig config;
    if (!config.isValid()) {
        config.items = {
            { "play", true },
            { "stop", true },
            { "record", true },
            { "rewind-start", true },
            { "rewind-end", true },
            { "loop", true },
            { "", true },
            { "automation", true },
            { "", true },
            { "zoom-in", true },
            { "zoom-out", true },
            { "fit-selection", true },
            { "fit-project", true },
            { "zoom", true },
            { "", true },
            { "spectral-editing", false },
            { "spectral-box-select", false },
            { "spectral-brush", false },
            { "", true },
            { "cut", true },
            { "copy", true },
            { "paste", true },
            { "", true },
            { "trim-audio-outside-selection", true },
            { "silence-audio-selection", true },
            { "", true },
            { "metronome", false },
            { "", true },
            { "playback-time", true },
            { "playback-bpm", false },
            { "playback-time-signature", false },
            { "", true },
            { "snap", true },
            { "", true },
            { "record-level", true },
            { "playback-level", true }
        };
    }
    return config;
}
