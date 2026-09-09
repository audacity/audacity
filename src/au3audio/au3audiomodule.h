/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>

#include "framework/global/modularity/imodulesetup.h"

namespace au::au3audio {
class Au3AudioEngine;
class Au3AudioDriverController;
class ISystemAudioDevicesListener;

class Au3AudioModule : public muse::modularity::IModuleSetup
{
public:

    std::string moduleName() const override;
    void registerExports() override;
    void onInit(const muse::IApplication::RunMode& mode) override;
    void onDeinit() override;

private:
    std::shared_ptr<Au3AudioEngine> m_audioEngine;
    std::shared_ptr<Au3AudioDriverController> m_audioDriverController;
    std::shared_ptr<ISystemAudioDevicesListener> m_systemAudioDevicesListener;
};
}
