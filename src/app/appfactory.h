/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <memory>

#include "global/iapplication.h"

#include "cmdoptions.h"

namespace au::app {
class AppFactory
{
public:
    AppFactory() = default;

    std::shared_ptr<muse::IApplication> newApp(const std::shared_ptr<AudacityCmdOptions>& options) const;

private:
    std::shared_ptr<muse::IApplication> newGuiApp(const std::shared_ptr<AudacityCmdOptions>& options) const;
    std::shared_ptr<muse::IApplication> newPluginRegistrationApp(const std::shared_ptr<AudacityCmdOptions>& options) const;
};
}
