/*
* Audacity: A Digital Audio Editor
*/
#include "videostubmodule.h"

#include <QtGlobal>

using namespace au::video;

static void video_init_qrc()
{
    Q_INIT_RESOURCE(video);
}

std::string VideoStubModule::moduleName() const
{
    return "video_stub";
}

void VideoStubModule::registerResources()
{
    // The stub QML module exists only so that ProjectPage.qml's unconditional
    // "import Audacity.Video" still resolves with the video module compiled
    // out. Without it the project page fails to load entirely.
    video_init_qrc();
}
