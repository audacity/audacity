/*
* Audacity: A Digital Audio Editor
*/
#include "playbackuiactions.h"

#include "framework/ui/view/iconcodes.h"
#include "framework/global/types/translatablestring.h"

#include "context/uicontext.h"
#include "context/shortcutcontext.h"

using namespace au::playback;
using namespace muse;
using namespace muse::ui;
using namespace muse::actions;

static const ActionQuery PLAYBACK_PLAY_QUERY("action://playback/play");
static const ActionQuery PLAYBACK_PAUSE_QUERY("action://playback/pause");
static const ActionQuery PLAYBACK_STOP_QUERY("action://playback/stop");

static const ActionQuery PLAYBACK_REWIND_START_QUERY("action://playback/rewind-start");
static const ActionQuery PLAYBACK_REWIND_END_QUERY("action://playback/rewind-end");

static const ActionQuery PLAYBACK_CHANGE_AUDIO_API_QUERY("action://playback/change-api");
static const ActionQuery PLAYBACK_CHANGE_PLAYBACK_DEVICE_QUERY("action://playback/change-playback-device");
static const ActionQuery PLAYBACK_CHANGE_RECORDING_DEVICE_QUERY("action://playback/change-recording-device");
static const ActionQuery PLAYBACK_CHANGE_INPUT_CHANNELS_QUERY("action://playback/change-input-channels");

static const ActionQuery PLAYBACK_LEVEL_QUERY("action://playback/level");

const UiActionList PlaybackUiActions::m_mainActions = {
    UiAction(PLAYBACK_PLAY_QUERY.toString(),
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_PLAYBACK,
             TranslatableString("action", "Play"),
             TranslatableString("action", "Play"),
             IconCode::Code::PLAY_FILL
             ),
    UiAction(PLAYBACK_PAUSE_QUERY.toString(),
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_PLAYBACK,
             TranslatableString("action", "Pause"),
             TranslatableString("action", "Pause"),
             IconCode::Code::PAUSE_FILL
             ),
    UiAction(PLAYBACK_STOP_QUERY.toString(),
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_OPENED,
             TranslatableString("action", "Stop"),
             TranslatableString("action", "Stop playback"),
             IconCode::Code::STOP_FILL
             ),
    UiAction(PLAYBACK_REWIND_START_QUERY.toString(),
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_FOCUSED,
             TranslatableString("action", "Rewind to start"),
             TranslatableString("action", "Rewind to start"),
             IconCode::Code::REWIND_START_FILL
             ),
    UiAction(PLAYBACK_REWIND_END_QUERY.toString(),
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_FOCUSED,
             TranslatableString("action", "Rewind to end"),
             TranslatableString("action", "Rewind to end"),
             IconCode::Code::REWIND_END_FILL
             ),
    UiAction("toggle-loop-region",
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
             TranslatableString("action", "Open audio setup context menu"),
             IconCode::Code::CONFIGURE
             ),
    UiAction("audio-settings",
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_FOCUSED,
             TranslatableString("action", "Audio settings"),
             TranslatableString("action", "Open audio setup dialog")
             ),
    UiAction("rescan-devices",
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_FOCUSED,
             TranslatableString("action", "Rescan audio devices"),
             TranslatableString("action", "Rescan audio devices")
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
    UiAction(PLAYBACK_LEVEL_QUERY.toString(),
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_FOCUSED,
             TranslatableString("action", "Playback level"),
             TranslatableString("action", "Set playback level"),
             IconCode::Code::AUDIO
             ),
    UiAction(PLAYBACK_CHANGE_AUDIO_API_QUERY.toString(),
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_FOCUSED,
             TranslatableString("action", "Change audio host"),
             TranslatableString("action", "Change audio host"),
             Checkable::Yes
             ),
    UiAction(PLAYBACK_CHANGE_PLAYBACK_DEVICE_QUERY.toString(),
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_FOCUSED,
             TranslatableString("action", "Change playback device"),
             TranslatableString("action", "Change playback device"),
             Checkable::Yes
             ),
    UiAction(PLAYBACK_CHANGE_RECORDING_DEVICE_QUERY.toString(),
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_FOCUSED,
             TranslatableString("action", "Change recording device"),
             TranslatableString("action", "Change recording device"),
             Checkable::Yes
             ),
    UiAction(PLAYBACK_CHANGE_INPUT_CHANNELS_QUERY.toString(),
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_FOCUSED,
             TranslatableString("action", "Change input channels"),
             TranslatableString("action", "Change input channels"),
             Checkable::Yes
             ),
    UiAction("toggle-loop-region",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Toggle loop region"),
             TranslatableString("action", "Toggle loop region"),
             IconCode::Code::LOOP
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
    UiAction("set-loop-region-in-out",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Set loop region in out"),
             TranslatableString("action", "Set loop region in out")
             ),
    UiAction("toggle-selection-follows-loop-region",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Creating a loop also selects audio"),
             TranslatableString("action", "Creating a loop also selects audio"),
             Checkable::Yes
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

PlaybackUiActions::PlaybackUiActions(std::shared_ptr<PlaybackController> controller)
    : m_controller(controller)
{
}

void PlaybackUiActions::init()
{
    registerActions();

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
            PLAYBACK_PLAY_QUERY.toString(),
            PLAYBACK_PAUSE_QUERY.toString(),
            PLAYBACK_REWIND_START_QUERY.toString(),
            PLAYBACK_REWIND_END_QUERY.toString()
        };

        m_actionEnabledChanged.send(codes);
    });

    audioDevicesProvider()->apiChanged().onNotify(this, [this]() {
        actionCheckedChanged().send(ActionCodeList({ PLAYBACK_CHANGE_AUDIO_API_QUERY.toString() }));
    });

    audioDevicesProvider()->outputDeviceChanged().onNotify(this, [this]() {
        actionCheckedChanged().send(ActionCodeList({ PLAYBACK_CHANGE_PLAYBACK_DEVICE_QUERY.toString() }));
    });

    audioDevicesProvider()->inputDeviceChanged().onNotify(this, [this]() {
        actionCheckedChanged().send(ActionCodeList({ PLAYBACK_CHANGE_RECORDING_DEVICE_QUERY.toString() }));
    });

    audioDevicesProvider()->inputChannelsChanged().onNotify(this, [this]() {
        actionCheckedChanged().send(ActionCodeList({ PLAYBACK_CHANGE_INPUT_CHANNELS_QUERY.toString() }));
    });
}

const UiActionList& PlaybackUiActions::actionsList() const
{
    static UiActionList alist;
    if (alist.empty()) {
        alist.insert(alist.end(), m_mainActions.cbegin(), m_mainActions.cend());
        alist.insert(alist.end(), m_settingsActions.cbegin(), m_settingsActions.cend());
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

void PlaybackUiActions::registerActions()
{
    m_actions.clear();

    m_actions.insert(m_actions.end(), m_mainActions.begin(), m_mainActions.end());
    m_actions.insert(m_actions.end(), m_settingsActions.begin(), m_settingsActions.end());
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
