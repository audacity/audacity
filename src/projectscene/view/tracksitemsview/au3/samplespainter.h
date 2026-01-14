#pragma once

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "isamplespainter.h"

#include "au3wrap/au3types.h"
#include "WaveMetrics.h"

namespace au::projectscene {
class SamplesPainter : public ISamplesPainter, public muse::Injectable
{
    muse::Inject<au::context::IGlobalContext> globalContext{ this };

public:
    SamplesPainter(const muse::modularity::ContextPtr& ctx) : muse::Injectable(ctx) {}
    void paint(QPainter& painter, const trackedit::ClipKey& clipKey, const IWavePainter::Params& params) override;
};
}
