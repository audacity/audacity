#pragma once

#include <QObject>

namespace au::projectscene {
class ClipKey
{
    Q_GADGET

public:
    ClipKey();

    uintptr_t au3WaveTrackPtr = 0; // WaveTrack*
    uintptr_t au3WaveClipPtr = 0; // WaveClip*
};
}
