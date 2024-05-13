#pragma once

#include <QColor>

namespace au::projectscene {
struct WaveStyle
{
    QColor blankBrush;
    QColor samplePen;
    QColor sampleBrush;
    QColor rmsPen;
    QColor clippedPen;
    QColor highlight;
};
}
