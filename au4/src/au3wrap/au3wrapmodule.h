/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_AU3WRAP_PROJECTMODULE_H
#define AU_AU3WRAP_PROJECTMODULE_H

#include "modularity/imodulesetup.h"

namespace au::au3 {
class WxLogWrap;
class Au3Playback;
class Au3Record;
class Au3AudioDevicesProvider;
class Au3WrapModule : public muse::modularity::IModuleSetup
{
public:

    std::string moduleName() const override;
    void registerExports() override;
    void onInit(const muse::IApplication::RunMode& mode) override;
    void onDeinit() override;

private:

    WxLogWrap* m_wxLog = nullptr;

    std::shared_ptr<Au3Playback> m_playback;
    std::shared_ptr<Au3Record> m_record;
    std::shared_ptr<Au3AudioDevicesProvider> m_audioDevicesProvider;
};
}

#endif // AU_AU3WRAP_PROJECTMODULE_H
