/*
 * Audacity: A Digital Audio Editor
 */
#include "spectrogramhit.h"

namespace au::spectrogram {
SpectrogramHit SpectrogramHitFactory::createSpectrogramHit(int trackId, int channel, double spectrogramY, double spectrogramHeight)
{
    return SpectrogramHit{ trackId, channel, spectrogramY, spectrogramHeight };
}
} // namespace au::spectrogram
