#include "projectsceneuiactions.h"

#include "global/types/translatablestring.h"
#include "context/uicontext.h"
#include "context/shortcutcontext.h"

#include "log.h"

using namespace au::projectscene;
using namespace muse;
using namespace muse::ui;

const UiActionList& ProjectSceneUiActions::mainActions() const
{
    static UiActionList list = {
        UiAction("play",
                 au::context::UiCtxNotationOpened,
                 au::context::CTX_NOTATION_FOCUSED,
                 TranslatableString("action", "Play"),
                 TranslatableString("action", "Play"),
                 IconCode::Code::PLAY
                 ),
        UiAction("stop",
                 au::context::UiCtxNotationOpened,
                 au::context::CTX_NOTATION_OPENED,
                 TranslatableString("action", "Stop"),
                 TranslatableString("action", "Stop playback"),
                 IconCode::Code::STOP
                 ),
        UiAction("rewind",
                 au::context::UiCtxNotationOpened,
                 au::context::CTX_NOTATION_FOCUSED,
                 TranslatableString("action", "Rewind"),
                 TranslatableString("action", "Rewind"),
                 IconCode::Code::REWIND
                 ),
        UiAction("audio-setup",
                 au::context::UiCtxNotationOpened,
                 au::context::CTX_ANY,
                 TranslatableString("action", "Audio setup"),
                 TranslatableString("action", "Audio setup"),
                 IconCode::Code::CONFIGURE,
                 Checkable::Yes
                 ),
    };
    return list;
}

const UiActionList& ProjectSceneUiActions::actionsList() const
{
    return mainActions();
}

bool ProjectSceneUiActions::actionEnabled(const UiAction& act) const
{
    UNUSED(act);
    return true;
}

bool ProjectSceneUiActions::actionChecked(const UiAction& act) const
{
    UNUSED(act);
    return false;
}

muse::async::Channel<muse::actions::ActionCodeList> ProjectSceneUiActions::actionEnabledChanged() const
{
    return muse::async::Channel<muse::actions::ActionCodeList>();
}

muse::async::Channel<muse::actions::ActionCodeList> ProjectSceneUiActions::actionCheckedChanged() const
{
    return muse::async::Channel<muse::actions::ActionCodeList>();
}

const ToolConfig& ProjectSceneUiActions::defaultPlaybackToolBarConfig()
{
    static ToolConfig config;
    if (!config.isValid()) {
        config.items = {
            { "play", true },
            { "stop", true },
            { "rewind", true },
        };
    }
    return config;
}
