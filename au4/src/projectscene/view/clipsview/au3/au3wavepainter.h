/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../iwavepainter.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "trackedit/iselectioncontroller.h"

class WaveTrack;
class WaveClip;
class AudacityProject;
namespace au::projectscene {
class Au3WavePainter : public IWavePainter
{
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    Au3WavePainter() = default;

    void paint(QPainter& painter, const trackedit::ClipKey& clipKey, const Params& params) override;

private:
    AudacityProject& projectRef() const;
    void doPaint(QPainter& painter, const WaveTrack* track, const WaveClip* clip, const Params& params);
};
}
