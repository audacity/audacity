/*
 * Audacity: A Digital Audio Editor
 */
#include "lv2viewlauncher.h"

#include "au3-components/EffectInterface.h"
#include "au3-effects/Effect.h"
#include "au3-lv2/LV2EffectBase.h"
#include "au3-lv2/LV2Symbols.h"
#include "au3-lv2/LV2Utils.h"
#include "au3-realtime-effects/RealtimeEffectState.h"

#include "lv2/ui/ui.h"

using namespace au::effects;

muse::Ret Lv2ViewLauncher::showEffect(const EffectInstanceId& instanceId) const
{
    return doShowEffect(instanceId, EffectFamily::LV2);
}

void Lv2ViewLauncher::showRealtimeEffect(const RealtimeEffectStatePtr& state) const
{
    doShowRealtimeEffect(state);
}

namespace {
unsigned uiTypeSupported(const char* hostTypeUri, const char* uiTypeUri)
{
    if (strcmp(hostTypeUri, uiTypeUri) == 0 || strcmp(uiTypeUri, LV2_UI__X11UI) == 0) {
        return 1;
    }
    return 0;
}
}

bool Lv2ViewLauncher::vendorUiSupported(const EffectId& effectId) const
{
    using namespace LV2Symbols;

    const auto* effect = dynamic_cast<const LV2EffectBase*>(effectsProvider()->effect(effectId));
    if (!effect) {
        return false;
    }

    using LilvUIsPtr = Lilv_ptr<LilvUIs, lilv_uis_free>;
    const LilvUIsPtr uis { lilv_plugin_get_uis(&effect->mPlug) };
    if (!uis) {
        return false;
    }

    const LilvNodePtr hostUiType { lilv_new_uri(gWorld, LV2_UI__Qt6UI) };
    LILV_FOREACH(uis, iter, uis.get()) {
        const LilvUI* ui = lilv_uis_get(uis.get(), iter);
        const LilvNode* uiType = nullptr;
        if (lilv_ui_is_supported(ui, uiTypeSupported, hostUiType.get(), &uiType)) {
            return true;
        }
        if (lilv_ui_is_a(ui, node_ExternalUI) || lilv_ui_is_a(ui, node_ExternalUIOld)) {
            return true;
        }
    }

    return false;
}
