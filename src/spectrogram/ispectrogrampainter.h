/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "spectrogramtypes.h"

#include "framework/global/modularity/imoduleinterface.h"

#include <QPainter>

namespace au::spectrogram {
class ISpectrogramPainter : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(ISpectrogramPainter)
public:
    virtual ~ISpectrogramPainter() = default;

    /**
     * @param trackHeight excluding the track header height
     */
    virtual void paintClip(QPainter&, const ClipInfo&, int trackHeight, double viewportT0, double viewportT1, double zoom,
                           const SelectedRegion&) = 0;
};
}
