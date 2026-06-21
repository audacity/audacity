/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <vector>

#include <QImage>

#include "modularity/imoduleinterface.h"
#include "global/async/notification.h"
#include "global/io/path.h"
#include "trackedit/dom/clip.h"

#include "videopreviewtypes.h"

namespace au::videopreview {

class IVideoPreviewService : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IVideoPreviewService)

public:
    ~IVideoPreviewService() override = default;

    virtual void init() = 0;

    virtual void linkImportedVideo(const muse::io::path_t& sourcePath,
                                   const std::vector<au::trackedit::Clip>& clips,
                                   double sourceOriginProjectTime) = 0;

    virtual void setProjectTime(double seconds) = 0;

    virtual VideoPreviewState state() const = 0;
    virtual muse::String stateText() const = 0;
    virtual muse::io::path_t sourcePath() const = 0;
    virtual double aspectRatio() const = 0;
    virtual QImage currentFrame() const = 0;

    virtual muse::async::Notification frameChanged() const = 0;
    virtual muse::async::Notification stateChanged() const = 0;
    virtual muse::async::Notification linkChanged() const = 0;
};
}
