/*
 * Audacity: A Digital Audio Editor
 */
#include "au3spectrogrampainter.h"

#include "spectrogramtypes.h"
#include "./au3spectrogramtypes.h"
#include "./au3spectrogramclipchannelpainter.h"
#include "./au3spectrogramsettings.h"
#include "./au3trackspectrogramconfiguration.h"
#include "../spectrogramutils.h"

#include "au3wrap/internal/domaccessor.h"

#include "framework/global/log.h"

#include "au3-project/Project.h"
#include "au3-wave-track/WaveTrack.h"

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

void Au3SpectrogramPainter::paintClipChannel(QPainter& qPainter, const ClipChannelInfo& channelInfo, const ViewInfo& viewInfo,
                                             const SelectionInfo& selectionInfo)
{
    const auto au3Project = m_au3Project.lock();
    IF_ASSERT_FAILED(au3Project) {
        return;
    }

    au3::Au3WaveTrack* const waveTrack = au3::DomAccessor::findWaveTrack(*au3Project, au3::Au3TrackId { channelInfo.trackId });
    if (!waveTrack) {
        return;
    }

    auto& settings = Au3SpectrogramSettings::Get(*waveTrack);

    const auto [minFreq, maxFreq] = spectrogramBounds(Au3TrackSpectrogramConfiguration { settings }, waveTrack->GetRate());

    const SpectrogramTrackContext trackContext{
        settings,
        waveTrack->IsSelected(),
        minFreq,
        maxFreq
    };

    ::WaveClip* const clip = au3::DomAccessor::findWaveClip(waveTrack, static_cast<int64_t>(channelInfo.clipId)).get();
    if (!clip) {
        return;
    }

    const auto channelHeight = static_cast<int>(viewInfo.channelHeight);
    QImage image{ channelInfo.xPaintEnd - channelInfo.xPaintBegin, channelHeight, QImage::Format_RGB888 };
    const std::shared_ptr<WaveClipChannel> clipChannel = clip->GetChannel<WaveClipChannel>(channelInfo.channel);
    IF_ASSERT_FAILED(clipChannel) {
        return;
    }
    Au3SpectrogramClipChannelPainter::fillImage(image, viewInfo, selectionInfo, trackContext, *clipChannel);
    qPainter.drawImage(QPoint { channelInfo.xPaintBegin, 0 }, image);
}
}
