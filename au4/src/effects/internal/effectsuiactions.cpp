/*
* Audacity: A Digital Audio Editor
*/
#include "effectsuiactions.h"

#include "ui/view/iconcodes.h"
#include "context/uicontext.h"
#include "context/shortcutcontext.h"
#include "types/translatablestring.h"

using namespace au::effects;
using namespace muse;
using namespace muse::ui;
using namespace muse::actions;

const UiActionList EffectsUiActions::m_actions = {
    UiAction("effect-open",
             au::context::UiCtxAny,
             au::context::CTX_ANY
             )
};

const UiActionList& EffectsUiActions::actionsList() const
{
    return m_actions;
}

bool EffectsUiActions::actionEnabled(const UiAction&) const
{
    return true;
}

bool EffectsUiActions::actionChecked(const UiAction&) const
{
    return true;
}

muse::async::Channel<ActionCodeList> EffectsUiActions::actionEnabledChanged() const
{
    static muse::async::Channel<ActionCodeList> ch;
    return ch;
}

muse::async::Channel<ActionCodeList> EffectsUiActions::actionCheckedChanged() const
{
    static muse::async::Channel<ActionCodeList> ch;
    return ch;
}
