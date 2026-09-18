/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_VIDEO_VIDEOMODULE_H
#define AU_VIDEO_VIDEOMODULE_H

#include <memory>

#include "modularity/imodulesetup.h"

namespace au::video {
class VideoService;

class VideoModule : public muse::modularity::IModuleSetup
{
public:
    std::string moduleName() const override;
    void registerExports() override;
    void registerResources() override;
    void registerUiTypes() override;
    void onInit(const muse::IApplication::RunMode& mode) override;

    muse::modularity::IContextSetup* newContext(const muse::modularity::ContextPtr& ctx) const override;
};

//! Per project: one transport, one attached video.
class VideoContext : public muse::modularity::IContextSetup
{
public:
    VideoContext(const muse::modularity::ContextPtr& ctx)
        : muse::modularity::IContextSetup(ctx) {}

    void registerExports() override;
    void onInit(const muse::IApplication::RunMode& mode) override;
    void onDeinit() override;

private:
    std::shared_ptr<VideoService> m_service;
};
}

#endif // AU_VIDEO_VIDEOMODULE_H
