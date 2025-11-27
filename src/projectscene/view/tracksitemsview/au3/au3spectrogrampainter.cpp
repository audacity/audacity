/*
 * Audacity: A Digital Audio Editor
 */
#include "au3spectrogrampainter.h"

#include "au3wrap/internal/domaccessor.h"
#include "trackedit/itrackeditproject.h"

#include "framework/global/log.h"

#include "libraries/lib-project/Project.h"
#include "libraries/lib-wave-track/WaveClip.h"
#include "libraries/lib-wave-track/WaveTrack.h"
#include "libraries/lib-wave-track-settings/SpectrogramSettings.h"

namespace au::projectscene {
void Au3SpectrogramPainter::init()
{
    globalContext()->currentProjectChanged().onNotify(this, [this]{
        const auto project = globalContext()->currentProject();
        if (project) {
            m_au3Project = reinterpret_cast<au3::Au3Project*>(project->au3ProjectPtr())->shared_from_this();
        }
    });
}

void Au3SpectrogramPainter::paint(QPainter& qPainter, const trackedit::ClipKey& clipKey, const WaveMetrics& metrics,
                                  const ZoomInfo& zoomInfo,
                                  const SelectedRegion& selectedRegion)
{
    const auto au3Project = m_au3Project.lock();
    IF_ASSERT_FAILED(au3Project) {
        return;
    }

    au3::Au3WaveTrack* const track = au3::DomAccessor::findWaveTrack(*au3Project, au3::Au3TrackId { clipKey.trackId });
    if (!track) {
        return;
    }

    if (m_trackPainterMap.find(clipKey.trackId) == m_trackPainterMap.end()) {
        std::weak_ptr<WaveTrack> weakTrack = std::static_pointer_cast<WaveTrack>(track->shared_from_this());
        m_trackPainterMap.emplace(clipKey.trackId, Au3SpectrogramTrackPainter { std::move(weakTrack) });
    }

    auto& trackPainter = m_trackPainterMap.at(clipKey.trackId);
    IF_ASSERT_FAILED(!trackPainter.trackExpired()) {
        return;
    }

    const SpectrogramGlobalContext gc {
        metrics, zoomInfo, selectedRegion
    };

    trackPainter.paintClip(clipKey.itemId, qPainter, gc);
}
}
