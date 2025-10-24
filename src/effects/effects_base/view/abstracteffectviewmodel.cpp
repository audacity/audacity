/*
* Audacity: A Digital Audio Editor
*/
#include "abstracteffectviewmodel.h"

#include "playback/iplayer.h"

namespace au::effects {
AbstractEffectViewModel::AbstractEffectViewModel(QObject* parent)
    : QObject(parent)
{
}

void AbstractEffectViewModel::init()
{
    const auto player = playback()->player();
    IF_ASSERT_FAILED(player) {
        return;
    }
    player->playbackStatusChanged().onReceive(this, [this](auto) {
        emit isPreviewingChanged();
    });

    doInit();
}

bool AbstractEffectViewModel::isPreviewing() const
{
    const auto player = playback()->player();
    IF_ASSERT_FAILED(player) {
        return false;
    }
    return player->playbackStatus() == playback::PlaybackStatus::Running;
}

void AbstractEffectViewModel::startPreview()
{
    doStartPreview();
}

void AbstractEffectViewModel::stopPreview()
{
    playback()->player()->stop();
}

EffectInstanceId AbstractEffectViewModel::instanceId() const
{
    return m_instanceId;
}

void AbstractEffectViewModel::setInstanceId(EffectInstanceId newInstanceId)
{
    if (m_instanceId == newInstanceId) {
        return;
    }
    m_instanceId = newInstanceId;
    emit instanceIdChanged();
}
}
