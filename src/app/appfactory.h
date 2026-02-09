/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <memory>

#include "global/iapplication.h"
#include "commandlineparser.h"

namespace au::app {
class AppFactory
{
public:
    AppFactory() = default;

    std::shared_ptr<muse::IApplication> newApp(const CommandLineParser& parser) const;

private:
    std::shared_ptr<muse::IApplication> newGuiApp(const CommandLineParser::Options& options) const;
    std::shared_ptr<muse::IApplication> newPluginRegistrationApp(const CommandLineParser::AudioPluginRegistration& task) const;
};
}
