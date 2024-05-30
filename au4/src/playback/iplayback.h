#pragma once

#include "modularity/imoduleinterface.h"

#include "audiotypes.h"

//! NOTE Implemented in Au3Wrap
namespace au::playback {
class IPlayer;
class IAudioOutput;
class IPlayback : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IPlayback)

public:
    ~IPlayback() = default;

    //! NOTE At the moment SequenceId not used
    virtual std::shared_ptr<IPlayer> player(audio::TrackSequenceId id = -1) const = 0;

    virtual std::shared_ptr<IAudioOutput> audioOutput() const = 0;
};
}
