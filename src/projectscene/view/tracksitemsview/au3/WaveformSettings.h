#pragma once

#include "au3wrap/au3types.h"
#include "ClientData.h"

namespace au::projectscene {
class WaveformSettings final : public ClientData::Cloneable<>
{
public:
    //! Create waveform settings for the track on demand
    //! Mutative access to attachment even if the track argument is const
    static WaveformSettings& Get(const WaveTrack& track);

    static WaveformSettings& defaults();

    PointerType Clone() const override;

    bool isLinear() const { return true; }

    float dBRange{ 60.0 };
};
}
