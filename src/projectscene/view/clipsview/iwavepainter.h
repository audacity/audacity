#pragma once

#include <QColor>
#include <QRect>

#include "au3/WaveMetrics.h"
#include "au3wrap/au3types.h"
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

    virtual void paint(int channelIndex, QPainter& painter, const WaveMetrics& metrics, const Style& style,
                       const au::au3::Au3WaveTrack& track, const au::au3::Au3WaveClip& clip) = 0;
};
}
