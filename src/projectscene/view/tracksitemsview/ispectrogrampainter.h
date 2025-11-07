/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "spectrogramtypes.h"

#include "trackedit/trackedittypes.h"

#include "framework/global/modularity/imoduleinterface.h"

#include <QPainter>

namespace au::projectscene {
struct WaveMetrics;

class ISpectrogramPainter : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(ISpectrogramPainter)
public:
    virtual ~ISpectrogramPainter() = default;

    virtual void paint(QPainter&, const trackedit::ClipKey&, const WaveMetrics&, const ZoomInfo&, const SelectedRegion&) = 0;
};
}
