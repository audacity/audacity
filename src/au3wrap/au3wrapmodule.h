/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "iglobalconfiguration.h"
#include "modularity/imodulesetup.h"

class Au3BasicUI;

namespace au::au3 {
class WxLogWrap;
class Au3Playback;
class Au3Record;
class Au3AudioDevicesProvider;
class Au3WrapModule : public muse::modularity::IModuleSetup
{
    muse::Inject<muse::IGlobalConfiguration> globalConfiguration;
public:

    std::string moduleName() const override;
    void registerExports() override;
    void onInit(const muse::IApplication::RunMode& mode) override;
    void onDeinit() override;

private:

    WxLogWrap* m_wxLog = nullptr;

    std::shared_ptr<Au3AudioDevicesProvider> m_audioDevicesProvider;
    std::shared_ptr<Au3BasicUI> m_au3BasicUi;
};
}
