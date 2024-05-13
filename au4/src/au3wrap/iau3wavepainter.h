#pragma once

#include "modularity/imoduleinterface.h"
#include "processing/processingtypes.h"

namespace au::au3 {
class IAu3WavePainter : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IAu3WavePainter)
public:
    virtual ~IAu3WavePainter() = default;

    struct Params {
        QRect viewRect;
        double zoom = 0.0;
    };

    virtual void paint(QPainter& painter, const processing::ClipKey& clipKey, const Params& params) = 0;
};
}
