/*
* Audacity: A Digital Audio Editor
*/
#include "videopreviewmodule.h"

#include <QtQml>

#include "modularity/ioc.h"

#include "internal/videopreviewservice.h"
#include "view/videopreviewitem.h"
#include "view/videopreviewmodel.h"

using namespace au::videopreview;

static const std::string mname("videopreview");

static void videopreview_init_qrc()
{
    Q_INIT_RESOURCE(videopreview);
}

std::string VideoPreviewModule::moduleName() const
{
    return mname;
}

void VideoPreviewModule::registerResources()
{
    videopreview_init_qrc();
}

void VideoPreviewModule::registerUiTypes()
{
    qmlRegisterType<VideoPreviewModel>("Audacity.VideoPreview", 1, 0, "VideoPreviewModel");
    qmlRegisterType<VideoPreviewItem>("Audacity.VideoPreview", 1, 0, "VideoPreviewItem");
}

muse::modularity::IContextSetup* VideoPreviewModule::newContext(const muse::modularity::ContextPtr& ctx) const
{
    return new VideoPreviewContext(ctx);
}

void VideoPreviewContext::registerExports()
{
    m_service = std::make_shared<VideoPreviewService>(iocContext());
    ioc()->registerExport<IVideoPreviewService>(mname, m_service);
}

void VideoPreviewContext::onInit(const muse::IApplication::RunMode& mode)
{
    if (mode != muse::IApplication::RunMode::GuiApp) {
        return;
    }

    m_service->init();
}
