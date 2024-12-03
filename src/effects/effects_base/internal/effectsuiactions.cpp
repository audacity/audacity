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

    UiAction("realtimeeffect-add",
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_OPENED,
             TranslatableString("action", "Add realtime effect"),
             TranslatableString("action", "Add realtime effect")
             ),
    UiAction("realtimeeffect-remove",
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_OPENED,
             TranslatableString("action", "Remove realtime effect"),
             TranslatableString("action", "Remove realtime effect")
             ),
    UiAction("realtimeeffect-replace",
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_OPENED,
             TranslatableString("action", "Replace realtime effect"),
             TranslatableString("action", "Replace realtime effect")
             ),

    UiAction("action://effects/presets/apply",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "&Apply Presets")
             ),
    UiAction("action://effects/presets/save",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "&Save Presets…")
             ),
    UiAction("action://effects/presets/delete",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "&Delete Preset")
             ),
    UiAction("action://effects/presets/import",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "&Import…"),
             TranslatableString("action", "Import preset")
             ),
    UiAction("action://effects/presets/export",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "&Export…"),
             TranslatableString("action", "Export preset")
             )
};

EffectsUiActions::EffectsUiActions(std::shared_ptr<EffectsActionsController> controller)
    : m_controller{controller}
{
}

void EffectsUiActions::init()
{
    effectExecutionScenario()->lastProcessorIdChanged().onReceive(this, [this](const EffectId& effectId) {
        const auto it = std::find_if(m_actions.begin(), m_actions.end(), [](const UiAction& action) {
            return action.code == "repeat-last-effect";
        });
        IF_ASSERT_FAILED(it != m_actions.end()) {
            return;
        }
        const auto effectTitle = effectsProvider()->meta(effectId).title;
        it->title = REPEAT_LAST_EFFECT_TITLE.arg(effectTitle);
        m_actionsChanged.send({ *it });
    });
}

const UiActionList& EffectsUiActions::actionsList() const
{
    return m_actions;
}

bool EffectsUiActions::actionEnabled(const UiAction& action) const
{
    return m_controller->canReceiveAction(action.code);
}

bool EffectsUiActions::actionChecked(const UiAction&) const
{
    return false;
}

muse::async::Channel<UiActionList> EffectsUiActions::actionsChanged() const
{
    return m_actionsChanged;
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
