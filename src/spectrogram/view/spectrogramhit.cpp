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

SpectrogramHit* SpectrogramHitFactory::createSpectrogramHit(int trackId, int channel, double spectrogramY, double spectrogramHeight)
{
    return new SpectrogramHit{ trackId, channel, spectrogramY, spectrogramHeight };
}
} // namespace au::spectrogram
