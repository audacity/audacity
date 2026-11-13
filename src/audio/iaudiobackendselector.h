/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <string>
#include <vector>

#include "framework/global/async/notification.h"
#include "framework/global/modularity/imoduleinterface.h"

#include "audioconfigurationtypes.h"

namespace au::audio {
//! Which of the registered audio backends is active. The choice persists.
class IAudioBackendSelector : MODULE_GLOBAL_INTERFACE
{
    INTERFACE_ID(IAudioBackendSelector)

public:
    virtual ~IAudioBackendSelector() = default;

    virtual std::vector<std::string> backends() const = 0;
    virtual std::string activeBackend() const = 0;
    //! What the user sees backend `id` as
    virtual std::string backendTitle(const std::string& id) const = 0;

    //! Suspends the stream of the active backend, if any, activates `id`, and restores the stream as a change of the audio
    //! configuration does: e.g. monitoring restarts, playback doesn't. The owners of meters set them again on
    //! `activeBackendChanged`.
    virtual ApplyResult setActiveBackend(const std::string& id) = 0;
    virtual muse::async::Notification activeBackendChanged() const = 0;
};
}
