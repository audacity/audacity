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
    SpectrogramHit(int trackId, int channel, double spectrogramY, double spectrogramHeight, QObject* parent = nullptr);

    int trackId = -1;
    int channel = 0;
    double spectrogramY = 0;
    double spectrogramHeight = 0;
};

class SpectrogramHitFactory : public QObject
{
    Q_OBJECT
public:
    Q_INVOKABLE SpectrogramHit* createSpectrogramHit(int trackId, int channel, double spectrogramY, double spectrogramHeight);
};
}
