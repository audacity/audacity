/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "ispectrogrampainter.h"
#include "./au3spectrogramtrackpainter.h"

#include "context/iglobalcontext.h"
#include "au3wrap/au3types.h"

#include "framework/global/modularity/ioc.h"
#include "framework/global/async/asyncable.h"

#include <unordered_map>

namespace au::spectrogram {
class Au3SpectrogramPainter final : public ISpectrogramPainter, public muse::async::Asyncable
{
    muse::Inject<context::IGlobalContext> globalContext;

public:
    ~Au3SpectrogramPainter() override = default;

    void init();

    void paintClip(QPainter&, const ClipInfo&, int trackHeight, double viewportT0, double viewportT1, double zoom, const SelectedRegion&) override;

private:
    std::weak_ptr<au3::Au3Project> m_au3Project;
    std::unordered_map<int64_t /*track ID*/, Au3SpectrogramTrackPainter> m_trackPainterMap;
};
}
