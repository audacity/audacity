/*
 * Audacity: A Digital Audio Editor
 */
#include "au3spectrogramtrackpainter.h"

#include "au3wrap/internal/domaccessor.h"

#include "framework/global/log.h"

#include "libraries/lib-wave-track/WaveClip.h"
#include "libraries/lib-wave-track/WaveTrack.h"
#include "libraries/lib-wave-track-settings/SpectrogramSettings.h"

namespace au::spectrogram {
Au3SpectrogramTrackPainter::Au3SpectrogramTrackPainter(std::weak_ptr<WaveTrack> waveTrack)
    : m_waveTrack(std::move(waveTrack))
{
}

void Au3SpectrogramTrackPainter::paintClip(QPainter& qPainter, const ClipInfo& clipInfo, const ViewInfo& viewInfo,
                                           const SelectedRegion& selectedRegion)
{
    const auto waveTrack = m_waveTrack.lock();
    IF_ASSERT_FAILED(waveTrack) {
        return;
    }

    auto& settings = ::SpectrogramSettings::Own(*waveTrack);

    float minFreq, maxFreq;
    SpectrogramBounds::Get(*waveTrack).GetBounds(*waveTrack, minFreq, maxFreq);

    constexpr auto leftRightHeightRatio = 1.0; // For now at least.
    const SpectrogramTrackContext trackContext{
        settings,
        waveTrack->IsSelected(),
        minFreq,
        maxFreq,
        leftRightHeightRatio
    };

    const int trackHeight{ viewInfo.trackHeight };

    ::WaveClip* const clip = au3::DomAccessor::findWaveClip(waveTrack.get(), static_cast<int64_t>(clipInfo.clipId)).get();
    if (!clip) {
        return;
    }

    if (m_clipPainterMap.find(clip) == m_clipPainterMap.end()) {
        ClipChannelPainterVector painters;
        for (auto i = 0u; i < clip->NChannels(); ++i) {
            painters.push_back(std::make_unique<Au3SpectrogramClipChannelPainter>(clip->GetChannel<WaveClipChannel>(i)));
        }
        m_clipPainterMap.emplace(clip, std::move(painters));
    }

    auto& channelPainters = m_clipPainterMap.at(clip);
    const auto isStereo = channelPainters.size() == 2;
    const auto rightChannelHeight = static_cast<int>(std::round(trackHeight * leftRightHeightRatio / 2));
    for (auto i = 0u; i < channelPainters.size(); ++i) {
        const auto isRightChannel = i == 1u;
        const auto channelHeight = isStereo ? (isRightChannel ? rightChannelHeight : trackHeight - rightChannelHeight) : trackHeight;
        QImage image{ clipInfo.xPaintEnd - clipInfo.xPaintBegin, channelHeight, QImage::Format_RGB888 };
        channelPainters[i]->paint(image, viewInfo, selectedRegion, trackContext);
        const auto channelY = isStereo ? (isRightChannel ? trackHeight - rightChannelHeight : 0) : 0;
        qPainter.drawImage(QPoint { clipInfo.xPaintBegin, channelY }, image);
    }
}

bool Au3SpectrogramTrackPainter::trackExpired() const
{
    return m_waveTrack.expired();
}
}
