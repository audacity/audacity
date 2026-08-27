/*
* Audacity: A Digital Audio Editor
*/
#include "clouduiactions.h"

#include "context/shortcutcontext.h"
#include "context/uicontext.h"

using namespace au::au3cloud;

const muse::ui::UiActionList CloudUiActions::m_actions = {
    muse::ui::UiAction("audacity://cloud/open-project-page",
                       au::context::UiCtxAny,
                       au::context::CTX_ANY,
                       //: Action title: shown as a menu item or a button label; keep it short
                       muse::TranslatableString("action", "View project on audio.com"),
                       //: Action description: shown as a tooltip; can be a full sentence
                       muse::TranslatableString("action_description", "View project on audio.com")
                       ),
    muse::ui::UiAction("audacity://cloud/open-audio-page",
                       au::context::UiCtxAny,
                       au::context::CTX_ANY,
                       //: Action title: shown as a menu item or a button label; keep it short
                       muse::TranslatableString("action", "View on audio.com"),
                       //: Action description: shown as a tooltip; can be a full sentence
                       muse::TranslatableString("action_description", "View on audio.com")
                       )
};

const muse::ui::UiActionList& CloudUiActions::actionsList() const
{
    return m_actions;
}

bool CloudUiActions::actionEnabled(const muse::ui::UiAction&) const
{
    return true;
}

bool CloudUiActions::actionChecked(const muse::ui::UiAction&) const
{
    return false;
}

muse::async::Channel<muse::actions::ActionCodeList> CloudUiActions::actionEnabledChanged() const
{
    return m_actionEnabledChanged;
}

muse::async::Channel<muse::actions::ActionCodeList> CloudUiActions::actionCheckedChanged() const
{
    return m_actionCheckedChanged;
}
