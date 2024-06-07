/*
* Audacity: A Digital Audio Editor
*/
#include "playbackuiactions.h"

#include "ui/view/iconcodes.h"
#include "context/uicontext.h"
#include "context/shortcutcontext.h"
#include "types/translatablestring.h"

using namespace au::playback;
using namespace muse;
using namespace muse::ui;
using namespace muse::actions;

const UiActionList PlaybackUiActions::m_mainActions = {
    UiAction("play",
             au::context::UiCtxNotationOpened,
             au::context::CTX_NOTATION_FOCUSED,
             TranslatableString("action", "Play"),
             TranslatableString("action", "Play"),
             IconCode::Code::PLAY_FILL
             ),
    UiAction("stop",
             au::context::UiCtxNotationOpened,
             au::context::CTX_NOTATION_OPENED,
             TranslatableString("action", "Stop"),
             TranslatableString("action", "Stop playback"),
             IconCode::Code::STOP
             ),
    UiAction("rewind-start",
             au::context::UiCtxNotationOpened,
             au::context::CTX_NOTATION_FOCUSED,
             TranslatableString("action", "Rewind to start"),
             TranslatableString("action", "Rewind to start"),
             IconCode::Code::REWIND_START_FILL
             ),
    UiAction("rewind-end",
             au::context::UiCtxNotationOpened,
             au::context::CTX_NOTATION_FOCUSED,
             TranslatableString("action", "Rewind to end"),
             TranslatableString("action", "Rewind to end"),
             IconCode::Code::REWIND_END_FILL
             ),
    UiAction("loop",
             au::context::UiCtxNotationOpened,
             au::context::CTX_NOTATION_FOCUSED,
             TranslatableString("action", "Loop playback"),
             TranslatableString("action", "Toggle ‘Loop playback’"),
             IconCode::Code::LOOP,
             Checkable::Yes
             ),
    UiAction("audio-setup",
             au::context::UiCtxNotationOpened,
             au::context::CTX_NOTATION_FOCUSED,
             TranslatableString("action", "Audio setup"),
             TranslatableString("action", "Open audio setup dialog"),
             IconCode::Code::CONFIGURE
             ),
    UiAction("playback-level",
             au::context::UiCtxNotationOpened,
             au::context::CTX_NOTATION_FOCUSED,
             TranslatableString("action", "Playback level"),
             TranslatableString("action", "Set playback level"),
             IconCode::Code::AUDIO // todo
             ),
    UiAction("playback-time",
             au::context::UiCtxNotationOpened,
             au::context::CTX_NOTATION_FOCUSED,
             TranslatableString("action", "Playback time"),
             TranslatableString("action", "Set playback time"),
             IconCode::Code::AUDIO // todo
             ),
    UiAction("record",
             au::context::UiCtxNotationOpened,
             au::context::CTX_NOTATION_FOCUSED,
             TranslatableString("action", "Record"),
             TranslatableString("action", "Record"),
             IconCode::Code::RECORD_FILL
             ),
};

const UiActionList PlaybackUiActions::m_settingsActions = {
    UiAction("repeat",
             au::context::UiCtxAny,
             au::context::CTX_NOTATION_FOCUSED,
             TranslatableString("action", "Play repeats"),
             TranslatableString("action", "Play repeats"),
             IconCode::Code::PLAY_REPEATS,
             Checkable::Yes
             ),
    UiAction("pan",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Pan score automatically"),
             TranslatableString("action", "Pan score automatically during playback"),
             IconCode::Code::PAN_SCORE,
             Checkable::Yes
             ),
};

const UiActionList PlaybackUiActions::m_loopBoundaryActions = {
    UiAction("loop-in",
             au::context::UiCtxAny,
             au::context::CTX_NOTATION_FOCUSED,
             TranslatableString("action", "Set loop marker left"),
             TranslatableString("action", "Set loop marker left"),
             IconCode::Code::LOOP_IN
             ),
    UiAction("loop-out",
             au::context::UiCtxAny,
             au::context::CTX_NOTATION_FOCUSED,
             TranslatableString("action", "Set loop marker right"),
             TranslatableString("action", "Set loop marker right"),
             IconCode::Code::LOOP_OUT
             ),
};

PlaybackUiActions::PlaybackUiActions(std::shared_ptr<PlaybackController> controller)
    : m_controller(controller)
{
}

void PlaybackUiActions::init()
{
    m_controller->actionCheckedChanged().onReceive(this, [this](const ActionCode& code) {
        m_actionCheckedChanged.send({ code });
    });

    m_controller->isPlayAllowedChanged().onNotify(this, [this]() {
        ActionCodeList codes;

        for (const UiAction& action : actionsList()) {
            codes.push_back(action.code);
        }

        m_actionEnabledChanged.send(codes);
    });
}

const UiActionList& PlaybackUiActions::actionsList() const
{
    static UiActionList alist;
    if (alist.empty()) {
        alist.insert(alist.end(), m_mainActions.cbegin(), m_mainActions.cend());
        alist.insert(alist.end(), m_settingsActions.cbegin(), m_settingsActions.cend());
        alist.insert(alist.end(), m_loopBoundaryActions.cbegin(), m_loopBoundaryActions.cend());
    }
    return alist;
}

bool PlaybackUiActions::actionEnabled(const UiAction& act) const
{
    if (!m_controller->isPlayAllowed()) {
        return false;
    }

    if (!m_controller->canReceiveAction(act.code)) {
        return false;
    }

    return true;
}

bool PlaybackUiActions::actionChecked(const UiAction& act) const
{
    return m_controller->actionChecked(act.code);
}

muse::async::Channel<ActionCodeList> PlaybackUiActions::actionEnabledChanged() const
{
    return m_actionEnabledChanged;
}

muse::async::Channel<ActionCodeList> PlaybackUiActions::actionCheckedChanged() const
{
    return m_actionCheckedChanged;
}

const UiActionList& PlaybackUiActions::settingsActions()
{
    return m_settingsActions;
}

const UiActionList& PlaybackUiActions::loopBoundaryActions()
{
    return m_loopBoundaryActions;
}

const muse::ui::ToolConfig& PlaybackUiActions::defaultPlaybackToolConfig()
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
            { "envelope", true },
            { "zoomin", true },
            { "zoomout", true },
            { "fit-selection", true },
            { "fit-project", true },
            { "zoom", true },
            { "trim-audio-outside-selection", true },
            { "silence-audio-selection", true },
            { "", true },
            { "playback-time", true },
            { "", true },
            { "record-level", true },
            { "playback-level", true }
        };
    }
    return config;
}
