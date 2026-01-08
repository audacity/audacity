/*
 * Audacity: A Digital Audio Editor
 */
#include "effectpreviewhelper.h"

#include "au3-components/EffectInterface.h"

#include "playback/iplayer.h"

#include "framework/global/log.h"

namespace au::effects {
EffectPreviewHelper::EffectPreviewHelper(QObject* parent, EffectInstanceId instanceId)
    : QObject(parent)
    , muse::Injectable(muse::iocCtxForQmlObject(this))
    , m_instanceId(instanceId)
{
    IF_ASSERT_FAILED(m_instanceId >= 0) {
        LOGE() << "Invalid instance ID: " << m_instanceId;
    }
}

void EffectPreviewHelper::init()
{
    const auto player = playback()->player();
    IF_ASSERT_FAILED(player) {
        return;
    }

    player->playbackStatusChanged().onReceive(this, [this](auto) {
        emit isPreviewingChanged();
    });
}

void EffectPreviewHelper::startPreview()
{
    if (m_instanceId < 0) {
        LOGE() << "Cannot start preview: invalid instance ID";
        return;
    }

    EffectSettingsAccessPtr settingsAccess = instancesRegister()->settingsAccessById(m_instanceId);
    if (!settingsAccess) {
        LOGE() << "Cannot start preview: no settings access for instance " << m_instanceId;
        return;
    }

    settingsAccess->ModifySettings([this](EffectSettings& settings) {
        executionScenario()->previewEffect(m_instanceId, settings);
        return nullptr;
    });
}

void EffectPreviewHelper::stopPreview()
{
    const auto player = playback()->player();
    IF_ASSERT_FAILED(player) {
        return;
    }
    player->stop();
}

bool EffectPreviewHelper::isPreviewing() const
{
    const auto player = playback()->player();
    IF_ASSERT_FAILED(player) {
        return false;
    }
    return player->playbackStatus() == playback::PlaybackStatus::Running;
}
}
