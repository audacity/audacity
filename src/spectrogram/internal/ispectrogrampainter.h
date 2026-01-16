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

    virtual void paintClipChannel(QPainter&, const ClipChannelInfo&, const ViewInfo&, const SelectionInfo&) = 0;
};
}
