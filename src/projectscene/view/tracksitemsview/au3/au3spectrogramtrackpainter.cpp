/*
 * Audacity: A Digital Audio Editor
 */
#include "au3spectrogramtrackpainter.h"

#include "au3wrap/internal/domaccessor.h"

#include "framework/global/log.h"

#include "libraries/lib-wave-track/WaveClip.h"
#include "libraries/lib-wave-track/WaveTrack.h"
#include "libraries/lib-wave-track-settings/SpectrogramSettings.h"

namespace au::projectscene {
Au3SpectrogramTrackPainter::Au3SpectrogramTrackPainter(std::weak_ptr<WaveTrack> waveTrack)
    : m_waveTrack(std::move(waveTrack))
{
}

void Au3SpectrogramTrackPainter::paintClip(trackedit::ClipId clipId, QPainter& qPainter, int xBegin, int xEnd, int trackHeight,
                                           const SpectrogramGlobalContext& globalContext)
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

    ::WaveClip* const clip = au3::DomAccessor::findWaveClip(waveTrack.get(), clipId).get();
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
        QImage image{ xEnd - xBegin, channelHeight, QImage::Format_RGB888 };
        channelPainters[i]->paint(image, globalContext, trackContext);
        const auto channelY = isStereo ? (isRightChannel ? trackHeight - rightChannelHeight : 0) : 0;
        qPainter.drawImage(QPoint { xBegin, channelY }, image);
    }
}

bool Au3SpectrogramTrackPainter::trackExpired() const
{
    return m_waveTrack.expired();
}

bool Au3SpectrogramTrackPainter::hasClip(trackedit::ClipId clipId) const
{
    const auto track = m_waveTrack.lock();
    if (!track) {
        return false;
    }
    for (const std::shared_ptr<WaveClip>& interval : track->Intervals()) {
        if (interval->GetId() == clipId) {
            return true;
        }
    }
    return false;
}
}
