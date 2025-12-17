#include "abstractviewlauncher.h"
#include "log.h"
#include "au3-realtime-effects/RealtimeEffectState.h"
#include "au3-effects/Effect.h"

namespace au::effects {
namespace {
constexpr const char16_t* REALTIME_VIEWER_URI
    = u"audacity://effects/realtime_viewer?instanceId=%1&effectState=%2&sync=false&modal=false&floating=true";
}

muse::Ret AbstractViewLauncher::doShowEffect(int instanceId, EffectFamily family) const
{
    muse::UriQuery uri(DESTRUCTIVE_EFFECT_VIEWER_URI);
    uri.addParam("instanceId", muse::Val(instanceId));
    uri.addParam("effectFamily", muse::Val(family));
    const auto ret = interactive()->openSync(uri);
    return ret.ret;
}

void AbstractViewLauncher::doShowRealtimeEffect(const RealtimeEffectStatePtr& state) const
{
    IF_ASSERT_FAILED(state) {
        return;
    }
    const auto instance = std::dynamic_pointer_cast<effects::EffectInstance>(state->GetInstance());
    if (!instance) {
        LOGW() << "Could not get instance for " << state->GetID().ToStdString();
        return;
    }

    const auto instanceId = instance->id();

    const muse::UriQuery query{ muse::String(REALTIME_VIEWER_URI).arg(size_t(instanceId)).arg(size_t(state.get())) };

    // If the dialog for this specific instance is opened, just raise it.
    if (interactive()->isOpened(query).val) {
        // Note: at the time of writing, `raise` doesn't seem to be working for QML dialogs (although it does for QtWidget dialogs)
        // Some changes are needed in the Muse framework.
        interactive()->raise(query);
        return;
    }

    interactive()->open(query);
}

void AbstractViewLauncher::hideRealtimeEffect(const RealtimeEffectStatePtr& state) const
{
    IF_ASSERT_FAILED(state) {
        return;
    }
    const auto instance = std::dynamic_pointer_cast<effects::EffectInstance>(state->GetInstance());
    if (!instance) {
        LOGW() << "Could not get instance for " << state->GetID().ToStdString();
        return;
    }
    const auto instanceId = instance->id();

    const muse::UriQuery query{ muse::String(REALTIME_VIEWER_URI).arg(size_t(instanceId)).arg(size_t(state.get())) };

    if (interactive()->isOpened(query).val) {
        interactive()->close(query);
    }
}
}
