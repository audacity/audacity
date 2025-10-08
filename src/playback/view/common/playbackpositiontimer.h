#pragma once

#include "playback/iplayback.h"

#include "framework/global/modularity/ioc.h"
#include "framework/audio/common/iaudiothreadsecurer.h"

#include <QQuickItem>

namespace au::playback {
class PlaybackPositionTimer : public QQuickItem
{
    muse::Inject<IPlayback> playback;
    muse::Inject<muse::audio::IAudioThreadSecurer> audioThreadSecurer;

public:
    explicit PlaybackPositionTimer(QQuickItem* parent = nullptr);
    ~PlaybackPositionTimer();

private:
    void itemChange(ItemChange change, const ItemChangeData &value) override;
    void doBeforeRendering();

    QMetaObject::Connection m_beforeRenderingConnection{};
};
}
