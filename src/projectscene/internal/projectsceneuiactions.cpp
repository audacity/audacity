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

static UiActionList STATIC_ACTIONS = {
    UiAction("clip-gain",
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_OPENED,
             TranslatableString("action", "Clip gain"),
             TranslatableString("action", "Clip gain"),
             IconCode::Code::AUTOMATION
             ),
    UiAction("split-tool",
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_FOCUSED,
             TranslatableString("action", "Split tool"),
             TranslatableString("action", "Split tool"),
             IconCode::Code::SPLIT_TOOL
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
    UiAction("zoom-default",
             au::context::UiCtxProjectOpened,
             au::context::CTX_ANY,
             TranslatableString("action", "Zoom default"),
             TranslatableString("action", "Zoom default")
             ),
    UiAction("zoom-to-selection",
             au::context::UiCtxProjectOpened,
             au::context::CTX_ANY,
             TranslatableString("action", "Zoom to selection"),
             TranslatableString("action", "Zoom to selection"),
             IconCode::Code::FIT_SELECTION
             ),
    UiAction("zoom-to-fit-project",
             au::context::UiCtxProjectOpened,
             au::context::CTX_ANY,
             TranslatableString("action", "Zoom to fit project"),
             TranslatableString("action", "Zoom to fit project"),
             IconCode::Code::FIT_PROJECT
             ),
    UiAction("zoom-toggle",
             au::context::UiCtxProjectOpened,
             au::context::CTX_ANY,
             TranslatableString("action", "Zoom toggle"),
             TranslatableString("action", "Zoom toggle"),
             IconCode::Code::ZOOM_TOGGLE
             ),
    UiAction("center-view-on-playhead",
             au::context::UiCtxProjectOpened,
             au::context::CTX_ANY,
             TranslatableString("action", "Center view on playhead"),
             TranslatableString("action", "Center view on playhead")
             ),
    UiAction("action://trackedit/global-view-spectrogram",
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_FOCUSED,
             TranslatableString("action", "Toggle spectral view"),
             TranslatableString("action", "Toggle spectral view"),
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
    UiAction("toggle-update-display-while-playing",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Update display while playing"),
             TranslatableString("action", "Update display while playing"),
             Checkable::Yes
             ),
    UiAction("toggle-pinned-play-head",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Pinned play head"),
             TranslatableString("action", "Pinned play head"),
             Checkable::Yes
             ),
    UiAction("toggle-playback-on-ruler-click-enabled",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Click ruler to start playback"),
             TranslatableString("action", "Click ruler to start playback"),
             Checkable::Yes
             ),
    // clip
    UiAction("clip-properties",
             au::context::UiCtxUnknown,
             au::context::CTX_ANY,
             TranslatableString("action", "Clip properties"),
             TranslatableString("action", "Show clip properties")
             ),
    UiAction("clip-rename",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Rename clip"),
             TranslatableString("action", "Rename clip"),
             Checkable::Yes
             ),
    UiAction("action://delete",
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_OPENED,
             TranslatableString("action", "Delete"),
             TranslatableString("action", "Delete"),
             IconCode::Code::DELETE_TANK,
             Checkable::Yes
             ),
    UiAction("action://trackedit/clip/change-color-auto",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Follow track color"),
             TranslatableString("action", "Follow track color"),
             Checkable::Yes
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
    UiAction("sel-ext-left",
             au::context::UiCtxProjectOpened,
             muse::shortcuts::CTX_PROJECT_OPENED,
             TranslatableString("action", "Extend selection left"),
             TranslatableString("action", "Extend selection left")
             ),
    UiAction("sel-ext-right",
             au::context::UiCtxProjectOpened,
             muse::shortcuts::CTX_PROJECT_OPENED,
             TranslatableString("action", "Extend selection right"),
             TranslatableString("action", "Extend selection right")
             ),
    UiAction("sel-cntr-left",
             au::context::UiCtxProjectOpened,
             muse::shortcuts::CTX_PROJECT_OPENED,
             TranslatableString("action", "Contract selection from left"),
             TranslatableString("action", "Contract selection from left")
             ),
    UiAction("sel-cntr-right",
             au::context::UiCtxProjectOpened,
             muse::shortcuts::CTX_PROJECT_OPENED,
             TranslatableString("action", "Contract selection from right"),
             TranslatableString("action", "Contract selection from right")
             ),
    UiAction("clip-pitch-speed",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Pitch and speed"),
             TranslatableString("action", "Pitch and speed")
             ),
    UiAction("toggle-rms-in-waveform",
             au::context::UiCtxProjectOpened,
             muse::shortcuts::CTX_PROJECT_OPENED,
             TranslatableString("action", "Show RMS in waveform"),
             TranslatableString("action", "Show RMS in waveform"),
             Checkable::Yes
             ),
    UiAction("toggle-clipping-in-waveform",
             au::context::UiCtxProjectOpened,
             muse::shortcuts::CTX_PROJECT_OPENED,
             TranslatableString("action", "Show clipping in waveform"),
             TranslatableString("action", "Show clipping in waveform"),
             Checkable::Yes
             ),
    UiAction("action://projectscene/track-view-half-wave",
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_OPENED,
             TranslatableString("action", "Half-wave"),
             TranslatableString("action", "Half-wave"),
             Checkable::Yes
             ),
    UiAction("toggle-label-editor",
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_OPENED,
             TranslatableString("action", "Show label editor"),
             TranslatableString("action", "Show label editor")
             ),
    UiAction("realtime-effect-move-up",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Move realtime effect up"),
             TranslatableString("action", "Move realtime effect up")
             ),
    UiAction("realtime-effect-move-down",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Move realtime effect down"),
             TranslatableString("action", "Move realtime effect down")
             )
};

ProjectSceneUiActions::ProjectSceneUiActions(const muse::modularity::ContextPtr& ctx,
                                             std::shared_ptr<ProjectSceneActionsController> controller)
    : muse::Contextable(ctx), m_controller(controller)
{
    m_actions = STATIC_ACTIONS;
}

void ProjectSceneUiActions::init()
{
    const auto& colorInfos = configuration()->clipColorInfos();
    m_actions.clear();
    m_actions.reserve(2 * colorInfos.size() + STATIC_ACTIONS.size());

    for (const auto& info : colorInfos) {
        muse::Color resolved = configuration()->clipColor(info.index);

        UiAction clipColorAction;
        clipColorAction.code = muse::actions::ActionQuery(makeClipColorChangeAction(info.index)).toString();
        clipColorAction.uiCtx = context::UiCtxProjectOpened;
        clipColorAction.scCtx = context::CTX_PROJECT_FOCUSED;
        clipColorAction.description = muse::TranslatableString("action", "Change clip color");
        clipColorAction.title = muse::TranslatableString("action", "Change clip color");
        clipColorAction.iconCode = IconCode::Code::FRETBOARD_MARKER_CIRCLE_FILLED;
        clipColorAction.iconColor = QString::fromStdString(resolved.toString());
        clipColorAction.checkable = Checkable::Yes;

        m_actions.push_back(std::move(clipColorAction));

        UiAction trackColorAction;
        trackColorAction.code = muse::actions::ActionQuery(makeTrackColorChangeAction(info.index)).toString();
        trackColorAction.uiCtx = context::UiCtxProjectOpened;
        trackColorAction.scCtx = context::CTX_DISABLED;
        trackColorAction.description = muse::TranslatableString("action", "Change track color");
        trackColorAction.title = muse::TranslatableString("action", "Change track color");
        trackColorAction.iconCode = IconCode::Code::FRETBOARD_MARKER_CIRCLE_FILLED;
        trackColorAction.iconColor = QString::fromStdString(resolved.toString());
        trackColorAction.checkable = Checkable::Yes;

        m_actions.push_back(std::move(trackColorAction));
    }

    m_actions.insert(m_actions.end(), STATIC_ACTIONS.begin(), STATIC_ACTIONS.end());

    m_controller->actionCheckedChanged().onReceive(this, [this](const ActionCode& code) {
        m_actionCheckedChanged.send({ code });
    });
    m_controller->actionEnabledChanged().onReceive(this, [this](const ActionCode& code) {
        m_actionEnabledChanged.send({ code });
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
            { "action://playback/play", true },
            { "action://playback/stop", true },
            { "action://record/start", true },
            { "action://playback/rewind-start", true },
            { "action://playback/rewind-end", true },
            { "toggle-loop-region", true },
            { "", true },
            { "clip-gain", true },
            { "split-tool", true },
            { "action://trackedit/global-view-spectrogram", true },
            { "zoom-in", true },
            { "zoom-out", true },
            { "zoom-to-selection", true },
            { "zoom-to-fit-project", true },
            { "zoom-toggle", true },
            // { "spectral-box-select", false },
            // { "spectral-brush", false },
            // { "", true },
            { "split-cut", false },
            { "action://trackedit/cut", false },
            { "action://trackedit/copy", false },
            { "action://trackedit/paste-default", false },
            { "", true },
            { "trim-audio-outside-selection", true },
            { "silence-audio-selection", true },
            { "", true },
            // { "metronome", false },
            // { "", true },
            { "playback-time", true },
            { "playback-bpm", false },
            { "playback-time-signature", false },
            { "", true },
            { "snap", true },
            { "", true },
            { "action://record/level", true },
            { "action://playback/level", true }
        };
    }
    return config;
}
