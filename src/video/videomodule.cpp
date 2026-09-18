/*
* Audacity: A Digital Audio Editor
*/
#include "videomodule.h"

#include <QQmlEngine>
#include <QtQml>

#include "modularity/ioc.h"
#include "framework/interactive/iinteractiveuriregister.h"

#include "internal/videoservice.h"
#include "view/videopanelmenumodel.h"
#include "view/videopanelmodel.h"
#include "view/videosurfaceitem.h"

using namespace au::video;
using namespace muse;
using namespace muse::modularity;

static const std::string mname("video");

static void video_init_qrc()
{
    Q_INIT_RESOURCE(video);
}

std::string VideoModule::moduleName() const
{
    return mname;
}

void VideoModule::registerExports()
{
}

void VideoModule::registerResources()
{
    video_init_qrc();
}

void VideoModule::registerUiTypes()
{
    qmlRegisterType<VideoPanelModel>("Audacity.Video", 1, 0, "VideoPanelModel");
    qmlRegisterType<VideoSurfaceItem>("Audacity.Video", 1, 0, "VideoSurfaceItem");
    qmlRegisterType<VideoPanelMenuModel>("Audacity.Video", 1, 0, "VideoPanelMenuModel");

    auto ir = globalIoc()->resolve<muse::interactive::IInteractiveUriRegister>(mname);
    if (ir) {
        ir->registerQmlUri(muse::Uri("audacity://video/offset"),
                           "Audacity/Video/VideoOffsetDialog.qml");
    }
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

    m_service->init();
}

void VideoContext::onDeinit()
{
    if (m_service) {
        // Not detach(): that clears the project's recorded video and commits
        // the change, so quitting with a video attached would erase it. The
        // application closing is not the user detaching anything.
        m_service->detachWithoutClearingProject();
    }
}
