#include "abstractviewlauncher.h"
#include "log.h"
#include "libraries/lib-realtime-effects/RealtimeEffectState.h"
#include "libraries/lib-effects/Effect.h"

namespace au::effects {
namespace {
constexpr const char16_t* REALTIME_VIEWER_URI
    = u"audacity://effects/realtime_viewer?instanceId=%1&effectStateId=%2&sync=false&modal=false&floating=true";
}

void AbstractViewLauncher::doShowRealtimeEffect(const RealtimeEffectStatePtr& state) const
{
    IF_ASSERT_FAILED(state) {
        return;
    }
    const auto instance = std::dynamic_pointer_cast<effects::EffectInstance>(state->GetInstance());
    if (!instance) {
        LOGW() << "Could not get instance for " << state->GetPluginID().ToStdString();
        return;
    }
    const auto instanceId = instance->id();

    const muse::UriQuery query{ muse::String(REALTIME_VIEWER_URI).arg(size_t(instanceId)).arg(state->GetID()) };

    // If the dialog for this specific instance is opened, just raise it.
    if (interactive()->isOpened(query).val) {
        // Note: at the time of writing, `raise` doesn't seem to be working for QML dialogs (although it does for QtWidget dialogs)
        // Some changes are needed in the Muse framework.
        interactive()->raise(query);
        return;
    }

    // At the time of writing, despite the `alwaysOnTop: true` property set on the dialog, whenever a new dialog is spawned,
    // the other dialog isn't always on top anymore. UX-wise this is very confusing, so until this problem is solved in the framework,
    // we only allow one effect dialog open at all times.
    const muse::Uri genericUri { REALTIME_VIEWER_URI };
    if (interactive()->isOpened(genericUri).val) {
        interactive()->close(genericUri);
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
        LOGW() << "Could not get instance for " << state->GetPluginID().ToStdString();
        return;
    }
    const auto instanceId = instance->id();

    const muse::UriQuery query{ muse::String(REALTIME_VIEWER_URI).arg(size_t(instanceId)).arg(state->GetID()) };

    if (interactive()->isOpened(query).val) {
        interactive()->close(query);
    }
}

void AbstractViewLauncher::toggleShowRealtimeEffect(const RealtimeEffectStatePtr& state) const
{
    IF_ASSERT_FAILED(state) {
        return;
    }

    const auto instance = std::dynamic_pointer_cast<effects::EffectInstance>(state->GetInstance());
    if (!instance) {
        // This could happen if e.g. a VST plugin was uninstalled since the last run.
        LOGW() << "Could not get instance for " << state->GetPluginID().ToStdString();
        return;
    }
    const auto instanceId = instance->id();

    const muse::UriQuery query{ muse::String(REALTIME_VIEWER_URI).arg(size_t(instanceId)).arg(state->GetID()) };

    if (interactive()->isOpened(query).val) {
        interactive()->close(query);
    } else {
        showRealtimeEffect(state);
    }
}
}
