/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../iwavepainter.h"
#include "../isamplespainter.h"
#include "../irmspainter.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "au3wrap/au3types.h"

namespace au::projectscene {
class Au3WavePainter : public IWavePainter
{
    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<ISamplesPainter> samplesPainter;
    muse::Inject<IRMSPainter> minMaxRMSPainter;

public:
    Au3WavePainter() = default;
    void paint(QPainter& painter, const trackedit::ClipKey& clipKey, const Params& params) override;

private:
    au3::Au3Project& projectRef() const;
    void doPaint(QPainter& painter, const au3::Au3WaveTrack* track, const au3::Au3WaveClip* clip, const Params& params);

    std::optional<Snap> m_snap;
};
}
