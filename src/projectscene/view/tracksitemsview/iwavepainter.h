#pragma once

#include <QColor>
#include <QRect>

#include "modularity/imoduleinterface.h"
#include "trackedit/trackedittypes.h"
#include "./tracksitemsviewtypes.h"

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
        QColor envelopeBackground;
        QColor selectedEnvelopeBackground;
    };

    struct Params : PaintParams {
        double channelHeightRatio = 0.5;
        bool showRMS = false;
        bool showClipping = false;
        Style style;
        bool isLinear = true;
        double dbRange = -60.0;
        float verticalZoom = 1.0f;
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
