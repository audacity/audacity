/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../ispectrogrampainter.h"
#include "./au3spectrogramchannelpainter.h"

#include "context/iglobalcontext.h"
#include "au3wrap/au3types.h"

#include "framework/global/modularity/ioc.h"
#include "framework/global/async/asyncable.h"

#include <array>

namespace au::projectscene {
class Au3SpectrogramPainter final : public ISpectrogramPainter, public muse::async::Asyncable
{
    muse::Inject<context::IGlobalContext> globalContext;

public:
    ~Au3SpectrogramPainter() override = default;

    void init();

    void paint(QPainter&, const trackedit::ClipKey&, const WaveMetrics&, const ZoomInfo&, const SelectedRegion&) override;

private:
    void onProjectChanged(project::IAudacityProject& project);

    std::weak_ptr<au3::Au3Project> m_au3Project;
    std::array<std::unique_ptr<Au3SpectrogramChannelPainter>, 2> m_channelPainters;
};
}
