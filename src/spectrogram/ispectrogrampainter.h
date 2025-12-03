/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "spectrogramtypes.h"

#include "trackedit/trackedittypes.h"

#include "framework/global/modularity/imoduleinterface.h"

#include <QPainter>
#include <QRect>

namespace au::projectscene {
struct WaveMetrics;
}

namespace au::spectrogram {
class ISpectrogramPainter : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(ISpectrogramPainter)
public:
    virtual ~ISpectrogramPainter() = default;

    /**
     * @param trackHeight excluding the track header height
     */
    virtual void paintClip(QPainter&, int xBegin, int xEnd, int trackHeight, double viewportT0, double viewportT1, double zoom,
                           const trackedit::ClipKey&, const projectscene::WaveMetrics&, const SelectedRegion&) = 0;
};
}
