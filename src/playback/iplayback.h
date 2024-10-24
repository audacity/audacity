#pragma once

#include "modularity/imoduleinterface.h"

#include "au3audio/audiotypes.h" // IWYU pragma: export
#include "playbacktypes.h"

namespace au::playback {
class IPlayer;
class IAudioOutput;
class IPlayback : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IPlayback)

public:
    virtual ~IPlayback() = default;

    //! NOTE At the moment SequenceId not used
    virtual std::shared_ptr<IPlayer> player(TrackSequenceId id = -1) const = 0;

    virtual std::shared_ptr<IAudioOutput> audioOutput() const = 0;
};
}
