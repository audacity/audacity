#include "au3wavepainter.h"

#include <QColor>
#include <QPainter>
#include <QPen>

#include <wx/types.h>
#include <wx/utils.h>

#include "global/realfn.h"

#include "ClipInterface.h"
#include "WaveClip.h"
#include "WaveTrack.h"
#include "WaveMetrics.h"
#include "ZoomInfo.h"
#include "Envelope.h"
#include "FrameStatistics.h"
#include "WaveformScale.h"
#include "WaveMetrics.h"
#include "WaveformSettings.h"
#include "graphics/Color.h"

#include "waveform/WaveBitmapCache.h"
#include "waveform/WaveDataCache.h"

#include "libraries/lib-track/PendingTracks.h"

#include "au3wrap/internal/domaccessor.h"
#include "au3wrap/internal/domconverter.h"
#include "au3wrap/au3types.h"

using namespace au::au3;

constexpr double CLIPVIEW_WIDTH_MIN = 4; // px

using Style = au::projectscene::Au3WavePainter::Style;

namespace {
bool showIndividualSamples(const Au3WaveClip& clip, double zoom)
{
    const double sampleRate = clip.GetRate();
    const double stretchRatio = clip.GetStretchRatio();

    // Require at least 1/2 pixel per sample for drawing individual samples.
    const double threshold1 = 0.5 * sampleRate / stretchRatio;

    bool showIndividualSamples = zoom > threshold1;
    return showIndividualSamples;
}
}

using namespace au::projectscene;
using namespace au::au3;

Au3Project& Au3WavePainter::projectRef() const
{
    Au3Project* project = reinterpret_cast<Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
    return *project;
}

void Au3WavePainter::paint(QPainter& painter, const trackedit::ClipKey& clipKey, const Params& params)
{
    //! NOTE Please don't remove, need for debug
    // if (!(clipKey.trackId == 2 && clipKey.index == 0)) {
    //     return;
    // }
    // LOGD() << "trackId: " << clipKey.trackId << ", clip: " << clipKey.index;
    WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(track) {
        return;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(track, clipKey.clipId);
    if (!clip) {
        // A clip-replacement operation may be ongoing, it's okay to return ; a new paint event will be triggered when it's done.
        return;
    }

    doPaint(painter, track, clip.get(), params);
}

void Au3WavePainter::doPaint(QPainter& painter, const Au3WaveTrack* _track, const Au3WaveClip* clip, const Params& params)
{
    Au3WaveTrack* track = const_cast<Au3WaveTrack*>(_track);

    WaveMetrics wm;
    wm.zoom = params.zoom;
    wm.fromTime = params.fromTime;
    wm.toTime = params.toTime;

    // calculate selection area relative to the clip itself
    if (!muse::RealIsEqual(params.selectionStartTime, params.selectionEndTime)) {
        wm.selectionStartTime = params.selectionStartTime - clip->Start() + clip->GetTrimLeft();
        wm.selectionEndTime = params.selectionEndTime - clip->Start() + clip->GetTrimLeft();
    }

    const Geometry& g = params.geometry;

    const std::vector<double> channelHeight {
        g.height * params.channelHeightRatio,
        g.height * (1 - params.channelHeightRatio),
    };

    painter.setPen(Qt::NoPen);

    project::IAudacityProjectPtr prj = globalContext()->currentProject();
    auto viewState = prj->viewState();
    if (showIndividualSamples(*clip, params.zoom)) {
        if (!m_snap) {
            m_snap = viewState->getSnap();
        }
        viewState->setSnap(Snap{ SnapType::Samples, true, false });
    } else {
        viewState->setSnap(m_snap.value_or(Snap{ SnapType::Bar, false, false }));
        m_snap.reset();
    }

    wm.width = g.width;
    wm.left = g.left;
    wm.top = 0.0;

    const bool showSamples = showIndividualSamples(*clip, params.zoom);
    for (unsigned i = 0; i < clip->NChannels(); ++i) {
        wm.height = channelHeight[i];
        showSamples
            ? samplesPainter()->paint(i, painter, wm, params.style, *track, *clip)
            : minMaxRMSPainter()->paint(i, painter, wm, params.style, *track, *clip);
        wm.top += wm.height;
    }
}
