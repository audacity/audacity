/*
 * Audacity: A Digital Audio Editor
 */
#include "au3spectrogrampainter.h"

#include "au3wrap/internal/domaccessor.h"

#include "framework/global/log.h"

#include "libraries/lib-project/Project.h"
#include "libraries/lib-wave-track/WaveTrack.h"
#include "libraries/lib-wave-track-settings/SpectrogramSettings.h"
#include "spectrogramtypes.h"

namespace au::spectrogram {
void Au3SpectrogramPainter::init()
{
    globalContext()->currentProjectChanged().onNotify(this, [this]{
        const auto project = globalContext()->currentProject();
        if (project) {
            m_au3Project = reinterpret_cast<au3::Au3Project*>(project->au3ProjectPtr())->shared_from_this();
        }
    });
}

void Au3SpectrogramPainter::paintClip(QPainter& qPainter, const ClipInfo& clipInfo, const ViewInfo& viewInfo,
                                      const SelectionInfo& selectionInfo)
{
    const auto au3Project = m_au3Project.lock();
    IF_ASSERT_FAILED(au3Project) {
        return;
    }

    au3::Au3WaveTrack* const track = au3::DomAccessor::findWaveTrack(*au3Project, au3::Au3TrackId { clipInfo.trackId });
    if (!track) {
        return;
    }

    m_trackPainter.paintClip(qPainter, clipInfo, viewInfo, selectionInfo, *track);
}
}
