/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>

#include "modularity/imodulesetup.h"

namespace au::au3audio {
class Au3AudioEngine;
class Au3AudioDevicesProvider;
class Au3DeviceManager;

class Au3AudioModule : public muse::modularity::IModuleSetup
{
public:

    std::string moduleName() const override;
    void registerExports() override;
    void onInit(const muse::IApplication::RunMode& mode) override;
    void onDeinit() override;

    muse::modularity::IContextSetup* newContext(const muse::modularity::ContextPtr& ctx) const override;

private:

    std::shared_ptr<Au3AudioEngine> m_audioEngine;
    std::shared_ptr<Au3DeviceManager> m_deviceManager;
};

class Au3AudioContext : public muse::modularity::IContextSetup
{
public:
    Au3AudioContext(const muse::modularity::ContextPtr& ctx)
        : muse::modularity::IContextSetup(ctx) {}

    void registerExports() override;
    void onInit(const muse::IApplication::RunMode& mode) override;
    void onDeinit() override;

private:
    std::shared_ptr<Au3AudioDevicesProvider> m_audioDevicesProvider;
};
}
