/*
 * Audacity: A Digital Audio Editor
 */
#include "effectsuiactions.h"

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

static UiActionList STATIC_ACTIONS = {
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
             TranslatableString("action", "Replace realtime effect"),
             muse::ui::Checkable::Yes
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

void EffectsUiActions::reload()
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

    auto makeActions = [this](const EffectMetaList& effects) {
        m_actions.clear();
        m_actions.reserve(effects.size() + STATIC_ACTIONS.size());

        for (const EffectMeta& e : effects) {
            UiAction action;
            action.code = makeEffectOpenAction(e.id).toString();
            action.uiCtx = context::UiCtxProjectOpened;
            action.scCtx = context::CTX_PROJECT_FOCUSED;
            action.description = TranslatableString::untranslatable(e.description);
            action.title = TranslatableString::untranslatable(e.title);

            m_actions.push_back(std::move(action));
        }

        m_actions.insert(m_actions.end(), STATIC_ACTIONS.begin(), STATIC_ACTIONS.end());
    };

    EffectMetaList metaList = effectsProvider()->effectMetaList();
    makeActions(metaList);

    effectsProvider()->effectMetaListChanged().onNotify(this, [this, makeActions]() {
        EffectMetaList metaList = effectsProvider()->effectMetaList();
        makeActions(metaList);
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
