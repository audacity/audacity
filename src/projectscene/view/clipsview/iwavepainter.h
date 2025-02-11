#pragma once

#include <QColor>
#include <QRect>

#include "modularity/imoduleinterface.h"
#include "trackedit/trackedittypes.h"

namespace au::projectscene {
class IWavePainter : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IWavePainter)
public:
    virtual ~IWavePainter() = default;

    struct Style {
        QColor blankBrush;
        QColor normalBackground;
        QColor selectedBackground;
        QColor samplePen;
        QColor selectedSamplePen;
        QColor sampleBrush;
        QColor rmsPen;
        QColor clippedPen;
        QColor highlight;
        QColor centerLine;
        QColor sampleHead;
        QColor sampleStalk;
        QColor sampleHeadSelection;
        QColor sampleStalkSelection;
    };

    struct Geometry {
        double height = 0.0;
        double width = 0.0;
        double left = 0.0;
    };

    struct Params {
        Geometry geometry;
        double zoom = 0.0;
        double fromTime = 0.0;
        double toTime = 0.0;
        double selectionStartTime = 0.0;
        double selectionEndTime = 0.0;
        double channelHeightRatio = 0.5;
        Style style;
    };

    enum class PlotType
    {
        MinMaxRMS,
        ConnectingDots,
        Stem
    };

    virtual void paint(QPainter& painter, const trackedit::ClipKey& clipKey, const Params& params,
                       std::optional<PlotType> plotType = std::nullopt) = 0;
};
}
