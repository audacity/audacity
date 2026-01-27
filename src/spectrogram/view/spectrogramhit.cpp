/*
 * Audacity: A Digital Audio Editor
 */
#include "spectrogramhit.h"

namespace au::spectrogram {
SpectrogramHit::SpectrogramHit(int trackId, int channel, double spectrogramY, double spectrogramHeight)
    : QObject(nullptr),
    trackId(trackId),
    channel(channel),
    spectrogramY(spectrogramY),
    spectrogramHeight(spectrogramHeight)
{
}

SpectrogramHit::SpectrogramHit(const SpectrogramHit& other)
    : QObject(nullptr),
    trackId(other.trackId),
    channel(other.channel),
    spectrogramY(other.spectrogramY),
    spectrogramHeight(other.spectrogramHeight)
{
}

SpectrogramHit* SpectrogramHitFactory::createSpectrogramHit(int trackId, int channel, double spectrogramY, double spectrogramHeight)
{
    auto hit = new SpectrogramHit{ trackId, channel, spectrogramY, spectrogramHeight };
    QQmlEngine::setObjectOwnership(hit, QQmlEngine::CppOwnership);
    return hit;
}
} // namespace au::spectrogram
