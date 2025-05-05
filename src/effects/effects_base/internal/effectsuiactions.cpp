/*
 * Audacity: A Digital Audio Editor
 */
#include "effectsuiactions.h"

#include "context/uicontext.h"
#include "context/shortcutcontext.h"
#include "types/translatablestring.h"
#include "log.h"

#include <unordered_map>

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
    UiAction("realtimeeffect-remove",
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_OPENED,
             TranslatableString("action", "Remove realtime effect"),
             TranslatableString("action", "Remove realtime effect")
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
             TranslatableString("action", "&Delete Preset"),
             IconCode::Code::DELETE_TANK
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

namespace {
UiAction makeUiAction(const char16_t* uri, const EffectMeta& meta)
{
    UiAction action;
    action.code = makeEffectAction(uri, meta.id).toString();
    action.uiCtx = au::context::UiCtxProjectOpened;
    action.scCtx = au::context::CTX_PROJECT_FOCUSED;
    action.description = TranslatableString::untranslatable(meta.description);
    action.title = TranslatableString::untranslatable(meta.title);
    return action;
}

// It can be that different plugins have the same name. Seeing them side by side in a menu is confusing for the user.
// To mitigate this, we replace the title with the path of the plugin.
void replaceIdenticalTitlesWithPaths(EffectMetaList& effects)
{
    using DuplicationKey = std::tuple<EffectFamily, EffectType, muse::String, muse::String>;

    std::map<DuplicationKey, std::vector<size_t> > duplicateMap;

    for (auto i = 0u; i < effects.size(); ++i) {
        const auto& effect = effects[i];
        DuplicationKey key{ effect.family, effect.type, effect.category, effect.title };
        duplicateMap[key].push_back(i);
    }

    for (const auto&[_, indices] : duplicateMap) {
        if (indices.size() == 1) {
            continue;
        }
        for (const size_t index : indices) {
            auto& meta = effects[index];
            meta.title = meta.path.toString();
        }
    }
}
}

void EffectsUiActions::makeActions(EffectMetaList effects)
{
    m_actions.clear();
    m_actions.reserve(effects.size() + STATIC_ACTIONS.size());

    replaceIdenticalTitlesWithPaths(effects);

    for (const EffectMeta& e : effects) {
        m_actions.push_back(makeUiAction(EFFECT_OPEN_ACTION, e));
        if (e.isRealtimeCapable) {
            for (const auto uri : { REALTIME_EFFECT_ADD_ACTION, REALTIME_EFFECT_REPLACE_ACTION }) {
                m_actions.push_back(makeUiAction(uri, e));
            }
        }
    }

    m_actions.insert(m_actions.end(), STATIC_ACTIONS.begin(), STATIC_ACTIONS.end());
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

    EffectMetaList metaList = effectsProvider()->effectMetaList();
    makeActions(metaList);

    effectsProvider()->effectMetaListChanged().onNotify(this, [this] {
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
