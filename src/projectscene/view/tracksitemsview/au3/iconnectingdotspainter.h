#pragma once

#include "modularity/imoduleinterface.h"
#include "trackedit/trackedittypes.h"

#include "iau3wavepainter.h"

namespace au::projectscene {
class IConnectingDotsPainter : public IAu3WavePainter, MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IConnectingDotsPainter)
};
}
