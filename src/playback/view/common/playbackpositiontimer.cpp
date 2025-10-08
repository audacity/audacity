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
    const auto playback = this->playback();
    if (!playback) {
        return;
    }
    const auto player = playback->player();
    if (!player) {
        return;
    }
    player->updatePlaybackPositionTimeCritical();
}
}
