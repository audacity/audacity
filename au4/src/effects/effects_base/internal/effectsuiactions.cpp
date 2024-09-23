/*
* Audacity: A Digital Audio Editor
*/
#include "effectsuiactions.h"

#include "ui/view/iconcodes.h"
#include "context/uicontext.h"
#include "context/shortcutcontext.h"
#include "types/translatablestring.h"

#include "log.h"

using namespace au::effects;
using namespace muse;
using namespace muse::ui;
using namespace muse::actions;

static const TranslatableString REPEAT_LAST_EFFECT_DEF_TITLE("action", "Repeat last effect");
static const TranslatableString REPEAT_LAST_EFFECT_TITLE("action", "Repeat %1");

UiActionList EffectsUiActions::m_actions = {
    UiAction("effect-open",
             au::context::UiCtxAny,
             au::context::CTX_ANY
             ),
    UiAction("repeat-last-effect",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             REPEAT_LAST_EFFECT_DEF_TITLE,
             TranslatableString("action", "Repeat last effect")
             ),
};

EffectsUiActions::EffectsUiActions(std::shared_ptr<EffectsActionsController> controller)
    : m_controller{controller}
{
}

void EffectsUiActions::init()
{
    effectExecutionScenario()->lastProcessorIsNowAvailable().onNotify(this, [this]() {
        auto it = std::find_if(m_actions.begin(), m_actions.end(), [](const UiAction& action) {
            return action.code == "repeat-last-effect";
        });
        IF_ASSERT_FAILED(it != m_actions.end()) {
            return;
        }

        if (effectExecutionScenario()->lastProcessorIsAvailable()) {
            EffectMeta meta = effectExecutionScenario()->lastProcessor();
            it->title = REPEAT_LAST_EFFECT_TITLE.arg(meta.title);
        } else {
            it->title = REPEAT_LAST_EFFECT_DEF_TITLE;
        }

        m_actionsChanged.send({ *it });
    });
}

const UiActionList& EffectsUiActions::actionsList() const
{
    return m_actions;
}

muse::async::Channel<muse::ui::UiActionList> EffectsUiActions::actionsChanged() const
{
    return m_actionsChanged;
}

bool EffectsUiActions::actionEnabled(const UiAction& action) const
{
    return m_controller->canReceiveAction(action.code);
}

bool EffectsUiActions::actionChecked(const UiAction&) const
{
    return false;
}

muse::async::Channel<ActionCodeList> EffectsUiActions::actionEnabledChanged() const
{
    return m_controller->canReceiveActionsChanged();
}

muse::async::Channel<ActionCodeList> EffectsUiActions::actionCheckedChanged() const
{
    static muse::async::Channel<ActionCodeList> ch;
    return ch;
}
