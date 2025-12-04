/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "./au3spectrogramclipchannelpainter.h"
#include "./au3spectrogramtypes.h"

#include <QPainter>

#include <memory>
#include <unordered_map>
#include <vector>

class WaveClip;
class WaveTrack;

namespace au::spectrogram {
class Au3SpectrogramTrackPainter
{
public:
    explicit Au3SpectrogramTrackPainter(std::weak_ptr<WaveTrack>);

    bool trackExpired() const;
    void paintClip(QPainter&, const ClipInfo&, const ViewInfo&, const SelectionInfo&);

private:
    const std::weak_ptr<WaveTrack> m_waveTrack;
    using ClipChannelPainterVector = std::vector<std::unique_ptr<Au3SpectrogramClipChannelPainter> >;
    std::unordered_map<const ::WaveClip*, ClipChannelPainterVector> m_clipPainterMap;
};
}
