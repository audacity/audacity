/*
 * Audacity: A Digital Audio Editor
 */
#include "au3spectrogrampainter.h"

#include "spectrogramtypes.h"
#include "./au3spectrogramtypes.h"
#include "./au3spectrogramclipchannelpainter.h"

#include "au3wrap/internal/domaccessor.h"

#include "framework/global/log.h"

#include "au3-project/Project.h"
#include "au3-wave-track/WaveTrack.h"
#include "au3-wave-track-settings/SpectrogramSettings.h"

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

    au3::Au3WaveTrack* const waveTrack = au3::DomAccessor::findWaveTrack(*au3Project, au3::Au3TrackId { clipInfo.trackId });
    if (!waveTrack) {
        return;
    }

    auto& settings = ::SpectrogramSettings::Own(*waveTrack);

    float minFreq, maxFreq;
    SpectrogramBounds::Get(*waveTrack).GetBounds(*waveTrack, minFreq, maxFreq);

    const SpectrogramTrackContext trackContext{
        settings,
        waveTrack->IsSelected(),
        minFreq,
        maxFreq
    };

    const int trackHeight{ viewInfo.trackHeight };

    ::WaveClip* const clip = au3::DomAccessor::findWaveClip(waveTrack, static_cast<int64_t>(clipInfo.clipId)).get();
    if (!clip) {
        return;
    }

    const auto isStereo = clip->NChannels() == 2;
    const auto rightChannelHeight = static_cast<int>(std::round(trackHeight * (1 - viewInfo.channelHeightRatio)));
    for (auto i = 0u; i < clip->NChannels(); ++i) {
        const auto isRightChannel = i == 1u;
        const auto channelHeight = isStereo ? (isRightChannel ? rightChannelHeight : trackHeight - rightChannelHeight) : trackHeight;
        QImage image{ clipInfo.xPaintEnd - clipInfo.xPaintBegin, channelHeight, QImage::Format_RGB888 };
        const std::shared_ptr<WaveClipChannel> clipChannel = clip->GetChannel<WaveClipChannel>(i);
        Au3SpectrogramClipChannelPainter::fillImage(image, viewInfo, selectionInfo, trackContext, *clipChannel);
        const auto channelY = isStereo ? (isRightChannel ? trackHeight - rightChannelHeight : 0) : 0;
        qPainter.drawImage(QPoint { clipInfo.xPaintBegin, channelY }, image);
    }
}
}
