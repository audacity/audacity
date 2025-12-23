/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "internal/ispectrogrampainter.h"

#include "context/iglobalcontext.h"
#include "au3wrap/au3types.h"

#include "framework/global/modularity/ioc.h"
#include "framework/global/async/asyncable.h"

namespace au::spectrogram {
class Au3SpectrogramPainter final : public ISpectrogramPainter, public muse::async::Asyncable
{
    muse::Inject<context::IGlobalContext> globalContext;

public:
    ~Au3SpectrogramPainter() override = default;

    void init();

    void paintClipChannel(QPainter&, const ClipChannelInfo&, const ViewInfo&, const SelectionInfo&) override;

private:
    std::weak_ptr<au3::Au3Project> m_au3Project;
};
}
