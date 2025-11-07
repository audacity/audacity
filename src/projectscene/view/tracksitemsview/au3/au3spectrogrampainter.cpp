/*
 * Audacity: A Digital Audio Editor
 */
#include "au3spectrogrampainter.h"

#include "au3wrap/internal/domaccessor.h"

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
            onProjectChanged(*project);
        }
    });
}

void Au3SpectrogramPainter::onProjectChanged(project::IAudacityProject& project)
{
    m_au3Project = reinterpret_cast<au3::Au3Project*>(project.au3ProjectPtr())->shared_from_this();
    for (auto& painter : m_channelPainters) {
        painter = std::make_unique<Au3SpectrogramChannelPainter>(m_au3Project);
    }
}

void Au3SpectrogramPainter::paint(QPainter& painter, const trackedit::ClipKey& clipKey, const WaveMetrics& metrics, const ZoomInfo& zoomInfo,
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

    auto& settings = SpectrogramSettings::Get(*track);

    const std::shared_ptr<au3::Au3WaveClip> clip = au3::DomAccessor::findWaveClip(track, clipKey.itemId);
    if (!clip) {
        return;
    }

    const Au3SpectrogramChannelPainter::Params params {
        settings,
        selectedRegion,
        zoomInfo,
        track->IsSelected()
    };

    for (const std::shared_ptr<WaveClipChannel> channel : clip->Channels()) {
        IF_ASSERT_FAILED(channel->GetChannelIndex() < m_channelPainters.size()) {
            continue;
        }
        m_channelPainters[channel->GetChannelIndex()]->paint(painter, *channel, metrics, params);
    }
}
}
