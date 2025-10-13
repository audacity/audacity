#include "playbackpositiontimer.h"

#include "playback/iplayer.h"
#include "global/log.h"

#include <QQuickWindow>

namespace au::playback {
PlaybackPositionTimer::PlaybackPositionTimer(QQuickItem* parent)
    : QQuickItem(parent)
{
    setFlag(ItemHasContents, true);
}

PlaybackPositionTimer::~PlaybackPositionTimer()
{
    disconnect(m_beforeSynchronizingConnection);
    m_beforeSynchronizingConnection = {};
}

void PlaybackPositionTimer::componentComplete()
{
    QQuickItem::componentComplete();

    // Somewhat slower than the 60fps of the render thread: this will add a jitter of 10ms,
    // leading to a total frame rate of 1 / (1 / 60 + 1 / 100) ~= 40fps.
    // Should still look perfectly smooth.
    m_timer.setInterval(10);
    m_timer.setTimerType(Qt::PreciseTimer);

    m_timer.callOnTimeout([this]() {
        update();
    });

    const auto player = this->player();
    // By this time we must have a player ...
    IF_ASSERT_FAILED(player) {
        return;
    }
    player->playbackStatusChanged().onReceive(this, [this](PlaybackStatus status) {
        if (status == PlaybackStatus::Running) {
            m_timer.start();
        } else {
            m_timer.stop();
        }
    });
}

void PlaybackPositionTimer::itemChange(ItemChange change, const ItemChangeData& value)
{
    QQuickItem::itemChange(change, value);
    if (change == ItemSceneChange) {
        if (value.window) {
            // Ensuring a direct connection, as per recommendation of the Qt documentation on `beforeSynchronizing`:
            // > Warning: This signal is emitted from the scene graph rendering thread.
            // > If your slot function needs to finish before execution continues, you must make sure that
            // > the connection is direct (see Qt::ConnectionType).
            m_beforeSynchronizingConnection = connect(value.window, &QQuickWindow::beforeSynchronizing, this,
                                                      &PlaybackPositionTimer::doBeforeSynchronizing, Qt::DirectConnection);
        } else {
            disconnect(m_beforeSynchronizingConnection);
            m_beforeSynchronizingConnection = {};
        }
    }
}

void PlaybackPositionTimer::doBeforeSynchronizing()
{
    const auto player = this->player();
    if (player && player->playbackStatus() == PlaybackStatus::Running) {
        player->updatePlaybackPositionTimeCritical();
    }
}

std::shared_ptr<IPlayer> PlaybackPositionTimer::player() const
{
    const auto playback = this->playback();
    if (!playback) {
        return nullptr;
    }
    return playback->player();
}
}
