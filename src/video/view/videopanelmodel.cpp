/*
* Audacity: A Digital Audio Editor
*/
#include "videopanelmodel.h"

#include <QFileInfo>

#include "translation.h"

using namespace au::video;

VideoPanelModel::VideoPanelModel(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

void VideoPanelModel::init()
{
    if (videoService() == nullptr) {
        return;
    }

    videoService()->attachedChanged().onNotify(this, [this]() {
        emit stateChanged();
    });
    emit stateChanged();
}

void VideoPanelModel::attachVideo()
{
    if (videoService() == nullptr || interactive() == nullptr) {
        return;
    }

    // Same extension list the import dialog offers, so a file that can be
    // imported for its audio can also be attached for its picture.
    const std::string videoFileExt
        = "*.avi *.mp4 *.mkv *.mov *.flv *.wmv *.asf *.webm *.mpg *.mpeg "
          "*.m4v *.ts *.gxf *.mxf *.nut *.dv *.3gp *.3g2 *.mj2";

    const std::vector<std::string> filter {
        muse::trc("video", "Video files") + " (" + videoFileExt + ")"
    };

    const muse::io::path_t path = interactive()->selectOpeningFileSync(
        muse::trc("video", "Attach video"), muse::io::path_t(), filter);

    if (path.empty()) {
        return;
    }

    videoService()->attach(path.toStdString());
}

void VideoPanelModel::detachVideo()
{
    if (videoService() != nullptr) {
        videoService()->detach();
    }
}

bool VideoPanelModel::hasVideo() const
{
    // QML can evaluate a binding before init() runs, and a ContextInject is
    // only resolvable once the QQmlContext is set.
    return videoService() != nullptr && videoService()->isAttached();
}

QString VideoPanelModel::sourceName() const
{
    if (videoService() == nullptr) {
        return QString();
    }

    const std::string path = videoService()->attachedPath();
    if (path.empty()) {
        return QString();
    }
    return QFileInfo(QString::fromStdString(path)).fileName();
}

bool VideoPanelModel::sourceMismatch() const
{
    return videoService() != nullptr && videoService()->sourceMismatch();
}

QString VideoPanelModel::warningText() const
{
    if (!sourceMismatch()) {
        return QString();
    }

    // The path still resolved, so nothing else in the module would notice
    // that the material behind it changed.
    return muse::qtrc("video",
                      "This file does not match the one saved with the project. "
                      "It may have been replaced or re-encoded.");
}

QString VideoPanelModel::statusText() const
{
    if (videoService() == nullptr) {
        return QString();
    }

    // Checked before the resolution readout, so a file that opens but cannot
    // be displayed says why instead of showing its dimensions over a black
    // rectangle.
    const VideoError err = videoService()->lastError();
    if (err != VideoError::None) {
        return errorMessage(err);
    }

    if (!videoService()->isAttached()) {
        return muse::qtrc("video", "No video attached");
    }

    const VideoStreamInfo& info = videoService()->streamInfo();
    return QString("%1 x %2").arg(info.width).arg(info.height);
}
