/*
* Audacity: A Digital Audio Editor
*/
#include "videomodule.h"

#include "modularity/ioc.h"

#include "internal/videoservice.h"

using namespace au::video;
using namespace muse;
using namespace muse::modularity;

static const std::string mname("video");

std::string VideoModule::moduleName() const
{
    return mname;
}

void VideoModule::registerExports()
{
}

void VideoModule::registerResources()
{
}

void VideoModule::registerUiTypes()
{
}

void VideoModule::onInit(const IApplication::RunMode&)
{
}

IContextSetup* VideoModule::newContext(const muse::modularity::ContextPtr& ctx) const
{
    return new VideoContext(ctx);
}

// =====================================================
// VideoContext
// =====================================================

void VideoContext::registerExports()
{
    m_service = std::make_shared<VideoService>(iocContext());

    ioc()->registerExport<IVideoService>(mname, m_service);
}

void VideoContext::onInit(const IApplication::RunMode& mode)
{
    if (mode == IApplication::RunMode::AudioPluginRegistration) {
        return;
    }
}

void VideoContext::onDeinit()
{
    if (m_service) {
        m_service->detach();
    }
}
