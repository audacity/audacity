#pragma once

#include "../iau3wavepainter.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

class WaveTrack;
class WaveClip;
class AudacityProject;
namespace au::au3 {
class Au3WavePainter : public IAu3WavePainter
{
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    Au3WavePainter() = default;

    void paint(QPainter& painter, const processing::ClipKey& clipKey, const Params& params) override;

private:
    AudacityProject& projectRef() const;
    void doPaint(QPainter& painter, const WaveTrack* track, const WaveClip* clip, const Params& params);
};
}
