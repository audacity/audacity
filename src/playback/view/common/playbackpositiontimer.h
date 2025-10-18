#pragma once

#include "playback/iplayback.h"

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"

#include <QQuickItem>
#include <QTimer>

namespace au::playback {
class PlaybackPositionTimer : public QQuickItem, public muse::async::Asyncable
{
    muse::Inject<IPlayback> playback;

public:
    explicit PlaybackPositionTimer(QQuickItem* parent = nullptr);
    ~PlaybackPositionTimer();

private:
    void itemChange(ItemChange change, const ItemChangeData& value) override;
    void doBeforeSynchronizing();
    void componentComplete() override;
    std::shared_ptr<IPlayer> player() const;

    QMetaObject::Connection m_beforeSynchronizingConnection{};
    QTimer m_timer;
};
}
