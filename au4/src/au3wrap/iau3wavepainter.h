#pragma once

#include <QColor>
#include <QRect>

#include "modularity/imoduleinterface.h"
#include "processing/processingtypes.h"

namespace au::au3 {
class IAu3WavePainter : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IAu3WavePainter)
public:
    virtual ~IAu3WavePainter() = default;

    struct Style {
        QColor blankBrush;
        QColor samplePen;
        QColor sampleBrush;
        QColor rmsPen;
        QColor clippedPen;
        QColor highlight;
    };

    struct Params {
        QRect viewRect;
        double zoom = 0.0;
        Style style;
    };

    virtual void paint(QPainter& painter, const processing::ClipKey& clipKey, const Params& params) = 0;
};
}
