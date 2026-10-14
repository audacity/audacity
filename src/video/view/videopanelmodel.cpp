/*
* Audacity: A Digital Audio Editor
*/
#include "videopanelmodel.h"

#include <algorithm>
#include <cmath>

#include "settings.h"

#include <QFileInfo>

#include "translation.h"

using namespace au::video;

//! Kept next to the other per-module settings rather than in the project,
//! because it describes this installation's toolbar and not the edit.
static const muse::Settings::Key TOOLBAR_THUMBNAIL_HEIGHT("video", "videoToolbar/thumbnailHeight");
static constexpr int DEFAULT_TOOLBAR_HEIGHT = 44;

VideoPanelModel::VideoPanelModel(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
    // In the constructor rather than init(): the playback toolbar keeps its
    // own instance purely to read toolbarHeight and never calls init(), and
    // without this it would not hear the size chosen from the right-click
    // menu, so the toolbar row would not resize.
    muse::settings()->valueChanged(TOOLBAR_THUMBNAIL_HEIGHT).onReceive(this, [this](const muse::Val&) {
        emit toolbarHeightChanged();
    });
}

void VideoPanelModel::init()
{
    if (videoService() == nullptr) {
        return;
    }

    videoService()->attachedChanged().onNotify(this, [this]() {
        emit stateChanged();
    });

    videoService()->offsetChanged().onNotify(this, [this]() {
        emit offsetChanged();
    });

    emit stateChanged();
    emit offsetChanged();
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

int VideoPanelModel::toolbarHeight() const
{
    const muse::Val value = muse::settings()->value(TOOLBAR_THUMBNAIL_HEIGHT);
    const int height = value.isNull() ? DEFAULT_TOOLBAR_HEIGHT : value.toInt();

    // Clamped on read as well as on write, so a hand-edited settings file
    // cannot produce a thumbnail taller than the toolbar row that holds it.
    return std::clamp(height, MIN_TOOLBAR_HEIGHT, MAX_TOOLBAR_HEIGHT);
}

void VideoPanelModel::setToolbarHeight(int height)
{
    const int clamped = std::clamp(height, MIN_TOOLBAR_HEIGHT, MAX_TOOLBAR_HEIGHT);
    if (clamped == toolbarHeight()) {
        return;
    }

    muse::settings()->setSharedValue(TOOLBAR_THUMBNAIL_HEIGHT, muse::Val(clamped));
    emit toolbarHeightChanged();
}

double VideoPanelModel::offset() const
{
    return videoService() != nullptr ? videoService()->offset().to_double() : 0.0;
}

void VideoPanelModel::setOffset(double offset)
{
    if (videoService() != nullptr) {
        videoService()->setOffset(muse::secs_t(offset));
    }
}

QString VideoPanelModel::offsetText() const
{
    const double value = offset();
    if (std::fabs(value) < 5e-4) {
        return QString();
    }

    // Milliseconds, because that is the resolution the offset is set at and
    // a frame at 25 fps is only 40 ms wide.
    return QString("%1%2 s")
           .arg(value > 0.0 ? "+" : "-")
           .arg(std::fabs(value), 0, 'f', 3);
}

bool VideoPanelModel::needsFFmpeg() const
{
    if (videoService() == nullptr) {
        return false;
    }

    const VideoError err = videoService()->lastError();
    return err == VideoError::FFmpegNotFound || err == VideoError::FFmpegTooOld;
}

void VideoPanelModel::openFFmpegPreferences()
{
    if (interactive() == nullptr) {
        return;
    }

    // The download and locate controls live in the General page's FFmpeg
    // section, which is the same library audio import and export use.
    muse::UriQuery preferences("audacity://preferences");
    preferences.addParam("currentPageId", muse::Val("general"));

    interactive()->open(preferences);
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
