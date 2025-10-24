/*
 * Audacity: A Digital Audio Editor
 */
#include "au3audiometerfactory.h"
#include "auaudio/internal/auqttimer.h"

std::shared_ptr<au::au3::Meter> au::au3::createAudioMeter()
{
    auto playingTimer = std::make_unique<AuQtTimer>(Qt::PreciseTimer);
    auto stoppingTimer = std::make_unique<AuQtTimer>(Qt::VeryCoarseTimer);
    return std::make_shared<au3::Meter>(std::move(playingTimer), std::move(stoppingTimer));
}
