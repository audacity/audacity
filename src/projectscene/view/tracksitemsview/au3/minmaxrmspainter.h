#pragma once

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "iminmaxrmspainter.h"

#include "au3wrap/au3types.h"
#include "WaveMetrics.h"

namespace au::projectscene {
class MinMaxRMSPainter : public IMinMaxRMSPainter, public muse::Contextable
{
    muse::ContextInject<au::context::IGlobalContext> globalContext{ this };

public:
    MinMaxRMSPainter(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}
    void paint(QPainter& painter, const trackedit::ClipKey& clipKey, const IWavePainter::Params& params) override;

    ClipData clipData(const trackedit::ClipKey& clipKey) override;
    bool columns(const trackedit::ClipKey& clipKey, size_t channelIndex, const IWavePainter::Params& params, const WaveMetrics& metrics,
                 std::vector<WaveColumnVertex>& columns) override;

private:
    std::shared_ptr<au::au3::Au3WaveClip> findClip(const trackedit::ClipKey& clipKey) const;
};
}
