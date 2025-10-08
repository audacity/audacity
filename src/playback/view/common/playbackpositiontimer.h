#pragma once

#include "playback/iplayback.h"

#include "framework/global/modularity/ioc.h"

#include <QQuickItem>

namespace au::playback {
class PlaybackPositionTimer : public QQuickItem
{
    muse::Inject<IPlayback> playback;

public:
    explicit PlaybackPositionTimer(QQuickItem* parent = nullptr);
    ~PlaybackPositionTimer();

private:
    void itemChange(ItemChange change, const ItemChangeData& value) override;
    void doBeforeSynchronizing();

    QMetaObject::Connection m_beforeSynchronizingConnection{};
};
}
