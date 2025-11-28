#pragma once

#include <QColor>
#include <QRect>

#include "modularity/imoduleinterface.h"
#include "trackedit/trackedittypes.h"

namespace au::projectscene {
struct Geometry {
    double height = 0.0;
    double width = 0.0;
    double left = 0.0;
};

struct PaintParams {
    Geometry geometry;
    double zoom = 0.0;
    double fromTime = 0.0;
    double toTime = 0.0;
    double selectionStartTime = 0.0;
    double selectionEndTime = 0.0;
};
}
