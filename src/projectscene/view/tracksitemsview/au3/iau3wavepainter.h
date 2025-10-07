#pragma once

#include <QPainter>

#include "../iwavepainter.h"

namespace au::projectscene {
class IAu3WavePainter
{
public:
    virtual ~IAu3WavePainter() = default;
    virtual void paint(QPainter& painter, const trackedit::ClipKey& clipKey, const IWavePainter::Params& params) = 0;
};
}
