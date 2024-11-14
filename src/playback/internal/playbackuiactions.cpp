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

static const ActionCode PLAY_ACTION_CODE("play");
static const ActionCode PAUSE_ACTION_CODE("pause");
static const ActionCode STOP_ACTION_CODE("stop");

static const ActionCode REWIND_START_ACTION_CODE("rewind-start");
static const ActionCode REWIND_END_ACTION_CODE("rewind-end");
static const ActionCode LOOP_ACTION_CODE("loop");

const UiActionList PlaybackUiActions::m_mainActions = {
    UiAction(PLAY_ACTION_CODE,
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_FOCUSED,
             TranslatableString("action", "Play"),
             TranslatableString("action", "Play"),
             IconCode::Code::PLAY_FILL
             ),
    UiAction(PAUSE_ACTION_CODE,
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_FOCUSED,
             TranslatableString("action", "Pause"),
             TranslatableString("action", "Pause"),
             IconCode::Code::PAUSE_FILL
             ),
    UiAction(STOP_ACTION_CODE,
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_OPENED,
             TranslatableString("action", "Stop"),
             TranslatableString("action", "Stop playback"),
             IconCode::Code::STOP_FILL
             ),
    UiAction(REWIND_START_ACTION_CODE,
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_FOCUSED,
             TranslatableString("action", "Rewind to start"),
             TranslatableString("action", "Rewind to start"),
             IconCode::Code::REWIND_START_FILL
             ),
    UiAction(REWIND_END_ACTION_CODE,
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_FOCUSED,
             TranslatableString("action", "Rewind to end"),
             TranslatableString("action", "Rewind to end"),
             IconCode::Code::REWIND_END_FILL
             ),
    UiAction(LOOP_ACTION_CODE,
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_FOCUSED,
             TranslatableString("action", "Loop playback"),
             TranslatableString("action", "Toggle ‘Loop playback’"),
             IconCode::Code::LOOP,
             Checkable::Yes
             ),
    UiAction("audio-setup",
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_FOCUSED,
             TranslatableString("action", "Audio setup"),
             TranslatableString("action", "Open audio setup dialog"),
             IconCode::Code::CONFIGURE
             ),
    UiAction("metronome",
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_FOCUSED,
             TranslatableString("action", "Metronome"),
             TranslatableString("action", "Toggle metronome playback"),
             IconCode::Code::METRONOME,
             Checkable::Yes
             ),
    UiAction("playback-time",
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_FOCUSED,
             TranslatableString("action", "Timecode"),
             TranslatableString("action", "Set playback time"),
             IconCode::Code::CLOCK
             ),
    UiAction("playback-bpm",
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_FOCUSED,
             TranslatableString("action", "Tempo"),
             TranslatableString("action", "Set playback tempo"),
             IconCode::Code::BPM
             ),
    UiAction("playback-time-signature",
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_FOCUSED,
             TranslatableString("action", "Time signature"),
             TranslatableString("action", "Set playback time signature"),
             IconCode::Code::TIME_SIGNATURE
             ),
    UiAction("playback-level",
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_FOCUSED,
             TranslatableString("action", "Playback level"),
             TranslatableString("action", "Set playback level"),
             IconCode::Code::AUDIO
             ),
};

const UiActionList PlaybackUiActions::m_settingsActions = {
    UiAction("repeat",
             au::context::UiCtxAny,
             au::context::CTX_PROJECT_FOCUSED,
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
             au::context::CTX_PROJECT_FOCUSED,
             TranslatableString("action", "Set loop marker left"),
             TranslatableString("action", "Set loop marker left"),
             IconCode::Code::LOOP_IN
             ),
    UiAction("loop-out",
             au::context::UiCtxAny,
             au::context::CTX_PROJECT_FOCUSED,
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

    m_controller->isPlayingChanged().onNotify(this, [this]() {
        ActionCodeList codes= {
            PLAY_ACTION_CODE,
            PAUSE_ACTION_CODE,
            REWIND_START_ACTION_CODE,
            REWIND_END_ACTION_CODE
        };

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
