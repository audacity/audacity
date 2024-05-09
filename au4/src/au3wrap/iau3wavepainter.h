#pragma once

#include "modularity/imoduleinterface.h"
#include "processing/processingtypes.h"

namespace au::au3 {
class IAu3WavePainter : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IAu3WavePainter)
public:
    virtual ~IAu3WavePainter() = default;

    struct Zoom {
        double offset = 0.0;
        double zoom = 0.0;
    };

    struct Params {
        QRect viewRect;
        Zoom zoom;
    };

    virtual void paint(QPainter& painter, const processing::ClipKey& clipKey, const Params& params) = 0;
};
}
