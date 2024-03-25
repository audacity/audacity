#include "projectsceneuiactions.h"

#include "global/types/translatablestring.h"
#include "context/uicontext.h"
#include "context/shortcutcontext.h"

#include "log.h"

using namespace au::projectscene;
using namespace mu::ui;
using namespace mu;

const UiActionList& ProjectSceneUiActions::mainActions() const
{
    static UiActionList list = {
        UiAction("play",
                 mu::context::UiCtxNotationOpened,
                 mu::context::CTX_NOTATION_FOCUSED,
                 TranslatableString("action", "Play"),
                 TranslatableString("action", "Play"),
                 IconCode::Code::PLAY
                 ),
        UiAction("stop",
                 mu::context::UiCtxNotationOpened,
                 mu::context::CTX_NOTATION_OPENED,
                 TranslatableString("action", "Stop"),
                 TranslatableString("action", "Stop playback"),
                 IconCode::Code::STOP
                 ),
        UiAction("rewind",
                 mu::context::UiCtxNotationOpened,
                 mu::context::CTX_NOTATION_FOCUSED,
                 TranslatableString("action", "Rewind"),
                 TranslatableString("action", "Rewind"),
                 IconCode::Code::REWIND
                 )
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

mu::async::Channel<mu::actions::ActionCodeList> ProjectSceneUiActions::actionEnabledChanged() const
{
    return mu::async::Channel<mu::actions::ActionCodeList>();
}

mu::async::Channel<mu::actions::ActionCodeList> ProjectSceneUiActions::actionCheckedChanged() const
{
    return mu::async::Channel<mu::actions::ActionCodeList>();
}
