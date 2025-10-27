#include "abstractviewlauncher.h"
#include "log.h"
#include "libraries/lib-realtime-effects/RealtimeEffectState.h"
#include "libraries/lib-effects/Effect.h"

namespace au::effects {
namespace {
constexpr const char16_t* REALTIME_VIEWER_URI
    = u"audacity://effects/realtime_viewer?effectState=%1&sync=false&modal=false&floating=true";

static int gInitializationInstanceId = -1;
}

muse::Ret AbstractViewLauncher::doShowEffect(int instanceId, EffectFamily family) const
{
    muse::UriQuery uri(EFFECT_VIEWER_URI);
    uri.addParam("effectFamily", muse::Val(family));
    gInitializationInstanceId = instanceId;
    const auto ret = interactive()->openSync(uri);
    gInitializationInstanceId = -1;
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

    const muse::UriQuery query{ muse::String(REALTIME_VIEWER_URI).arg(size_t(state.get())) };

    // If the dialog for this specific instance is opened, just raise it.
    if (interactive()->isOpened(query).val) {
        // Note: at the time of writing, `raise` doesn't seem to be working for QML dialogs (although it does for QtWidget dialogs)
        // Some changes are needed in the Muse framework.
        interactive()->raise(query);
        return;
    }

    gInitializationInstanceId = instance->id();
    interactive()->open(query);
    gInitializationInstanceId = -1;
}

void AbstractViewLauncher::hideRealtimeEffect(const RealtimeEffectStatePtr& state) const
{
    IF_ASSERT_FAILED(state) {
        return;
    }
    const muse::UriQuery query{ muse::String(REALTIME_VIEWER_URI).arg(size_t(state.get())) };
    if (interactive()->isOpened(query).val) {
        interactive()->close(query);
    }
}

int AbstractViewLauncher::initializationInstanceId()
{
    return gInitializationInstanceId;
}
}
