/*
 * Audacity: A Digital Audio Editor
 */
#include "effectsuiactions.h"

#include "effectsutils.h"

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
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Repeat last effect")
             ),
    UiAction("realtimeeffect-remove",
             au::context::UiCtxProjectOpened,
             au::context::CTX_DISABLED,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Remove realtime effect"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Remove realtime effect")
             ),
    UiAction("action://effects/presets/apply",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "&Apply preset")
             ),
    UiAction("action://effects/presets/save_as",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "Save preset as…")
             ),
    UiAction("action://effects/presets/save",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "&Save preset")
             ),
    UiAction("action://effects/presets/delete",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "&Delete preset"),
             IconCode::Code::DELETE_TANK
             ),
    UiAction("action://effects/presets/import",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "&Import…"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Import preset")
             ),
    UiAction("action://effects/presets/export",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("action", "&Export…"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("action_description", "Export preset")
             ),
    UiAction("action://effects/toggle_vendor_ui",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             //: Action title: shown as a menu item or a button label; keep it short
             TranslatableString("effects", "Use vendor UI"),
             //: Action description: shown as a tooltip; can be a full sentence
             TranslatableString("effects", "Toggle between vendor UI and fallback UI"),
             Checkable::Yes
             )
};

EffectsUiActions::EffectsUiActions(const muse::modularity::ContextPtr& ctx, EffectsActionsController* controller)
    : muse::Contextable(ctx), m_controller{controller}
{
    effectExecutionScenario()->lastProcessorIdChanged().onReceive(this, [this](const EffectId& effectId) {
        const auto it = std::find_if(m_actions.begin(), m_actions.end(), [](const UiAction& action) {
            return action.code == "repeat-last-effect";
        });
        IF_ASSERT_FAILED(it != m_actions.end()) {
            return;
        }
        const auto effectTitle = utils::effectDisplayTitle(effectsProvider()->meta(effectId));
        it->title = REPEAT_LAST_EFFECT_TITLE.arg(effectTitle);
        m_actionsChanged.send({ *it });
    });

    effectsProvider()->effectMetaListChanged().onNotify(this, [this] {
        reload();
    });
}

namespace {
UiAction makeUiAction(const std::string& uri, const EffectMeta& meta)
{
    UiAction action;
    action.code = makeEffectAction(uri, meta.id);
    action.uiCtx = au::context::UiCtxProjectOpened;
    action.scCtx = au::context::CTX_PROJECT_FOCUSED;
    action.description = TranslatableString::untranslatable(meta.description);
    action.title = TranslatableString::untranslatable(utils::effectDisplayTitle(meta));
    return action;
}
}

void EffectsUiActions::makeActions(EffectMetaList effects)
{
    m_actions.clear();
    m_actions.reserve(effects.size() + STATIC_ACTIONS.size());

    utils::replaceIdenticalTitlesWithPaths(effects);

    for (const EffectMeta& e : effects) {
        m_actions.push_back(makeUiAction(EFFECT_OPEN_ACTION, e));
        if (e.isRealtimeCapable) {
            for (const auto& uri : { REALTIME_EFFECT_ADD_ACTION, REALTIME_EFFECT_REPLACE_ACTION }) {
                UiAction action = makeUiAction(uri, e);
                action.scCtx = au::context::CTX_DISABLED;
                m_actions.push_back(std::move(action));
            }
        }
    }

    m_actions.insert(m_actions.end(), STATIC_ACTIONS.begin(), STATIC_ACTIONS.end());
}

void EffectsUiActions::reload()
{
    EffectMetaList metaList = effectsProvider()->effectMetaList();
    makeActions(metaList);
}

const UiActionList& EffectsUiActions::actionsList() const
{
    return m_actions;
}

bool EffectsUiActions::actionEnabled(const UiAction& action) const
{
    return m_controller->canReceiveAction(action.code);
}

bool EffectsUiActions::actionChecked([[maybe_unused]] const UiAction& action) const
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
    return m_actionCheckedChanged;
}
