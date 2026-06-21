/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>
#include <string>

#include "modularity/imodulesetup.h"

namespace au::videopreview {

class VideoPreviewService;

class VideoPreviewModule : public muse::modularity::IModuleSetup
{
public:
    std::string moduleName() const override;

    void registerResources() override;
    void registerUiTypes() override;

    muse::modularity::IContextSetup* newContext(const muse::modularity::ContextPtr& ctx) const override;
};

class VideoPreviewContext : public muse::modularity::IContextSetup
{
public:
    explicit VideoPreviewContext(const muse::modularity::ContextPtr& ctx)
        : muse::modularity::IContextSetup(ctx)
    {
    }

    void registerExports() override;
    void onInit(const muse::IApplication::RunMode& mode) override;

private:
    std::shared_ptr<VideoPreviewService> m_service;
};
}
