/*
 * Audacity: A Digital Audio Editor
 */
#include "au3spectrogramtrackpainter.h"

#include "au3wrap/internal/domaccessor.h"

#include "framework/global/log.h"

#include "libraries/lib-wave-track/WaveClip.h"
#include "libraries/lib-wave-track/WaveTrack.h"

namespace au::projectscene {
Au3SpectrogramTrackPainter::Au3SpectrogramTrackPainter(std::weak_ptr<WaveTrack> waveTrack)
    : m_waveTrack(std::move(waveTrack))
{
}

void Au3SpectrogramTrackPainter::paintClip(trackedit::ClipId clipId, QPainter& qPainter, const SpectrogramGlobalContext& globalContext)
{
    const auto waveTrack = m_waveTrack.lock();
    IF_ASSERT_FAILED(waveTrack) {
        return;
    }

    auto& settings = ::SpectrogramSettings::Own(*waveTrack);

    float minFreq, maxFreq;
    SpectrogramBounds::Get(*waveTrack).GetBounds(*waveTrack, minFreq, maxFreq);

    const SpectrogramTrackContext trackContext{
        settings,
        waveTrack->IsSelected(),
        minFreq,
        maxFreq,
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

    auto& painters = m_clipPainterMap.at(clip);
    for (auto i = 0u; i < painters.size(); ++i) {
        painters[i]->paint(qPainter, globalContext, trackContext);
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
