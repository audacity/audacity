#pragma once

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "isamplespainter.h"

#include "au3wrap/au3types.h"
#include "WaveMetrics.h"

namespace au::projectscene {
class SamplesPainter : public ISamplesPainter
{
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    SamplesPainter() = default;
    void paint(QPainter& painter, const trackedit::ClipKey& clipKey, const IWavePainter::Params& params) override;
};
}
