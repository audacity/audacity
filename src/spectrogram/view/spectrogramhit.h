/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QObject>

namespace au::spectrogram {
class SpectrogramHit : public QObject
{
    Q_OBJECT

public:
    SpectrogramHit(int trackId, int channel, double spectrogramY, double spectrogramHeight);
    SpectrogramHit(const SpectrogramHit&);
    ~SpectrogramHit() override = default;

    const int trackId = -1;
    const int channel = 0;
    const double spectrogramY = 0; //! relative to the tracks container
    const double spectrogramHeight = 0;
};

class SpectrogramHitFactory : public QObject
{
    Q_OBJECT
public:
    Q_INVOKABLE SpectrogramHit* createSpectrogramHit(int trackId, int channel, double spectrogramY, double spectrogramHeight);
};
}
