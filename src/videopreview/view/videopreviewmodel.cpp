/*
* Audacity: A Digital Audio Editor
*/
#include "videopreviewmodel.h"

using namespace au::videopreview;

VideoPreviewModel::VideoPreviewModel(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

void VideoPreviewModel::init()
{
    if (m_inited || !service()) {
        return;
    }

    service()->stateChanged().onNotify(this, [this]() {
        emit stateChanged();
    });

    service()->frameChanged().onNotify(this, [this]() {
        emit frameChanged();
        emit aspectRatioChanged();
    });

    service()->linkChanged().onNotify(this, [this]() {
        emit linkChanged();
        emit stateChanged();
    });

    m_inited = true;
}

QString VideoPreviewModel::stateText() const
{
    return service() ? service()->stateText().toQString() : QString();
}

bool VideoPreviewModel::hasFrame() const
{
    return service() && !service()->currentFrame().isNull();
}

bool VideoPreviewModel::showStateText() const
{
    if (!service() || hasFrame()) {
        return false;
    }

    const VideoPreviewState state = service()->state();
    return state != VideoPreviewState::Empty && state != VideoPreviewState::Ready;
}

QString VideoPreviewModel::sourcePath() const
{
    return service() ? service()->sourcePath().toQString() : QString();
}

double VideoPreviewModel::aspectRatio() const
{
    return service() ? service()->aspectRatio() : 16.0 / 9.0;
}

bool VideoPreviewModel::available() const
{
    return service() && service()->state() == VideoPreviewState::Ready;
}
