/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <memory>

#include "framework/global/modularity/ioc.h"
#include "framework/global/async/asyncable.h"

#include "context/iglobalcontext.h"
#include "../iclipfades.h"
#include "../iprojecthistory.h"
#include "../itrackeditinteraction.h"

namespace au::trackedit {
class ClipFadeDrag;

class ClipFades : public IClipFades, public muse::async::Asyncable, public muse::Contextable
{
    muse::ContextInject<context::IGlobalContext> globalContext { this };
    muse::ContextInject<ITrackeditInteraction> trackeditInteraction { this };
    muse::ContextInject<IProjectHistory> projectHistory { this };

public:
    explicit ClipFades(const muse::modularity::ContextPtr& ctx);
    ~ClipFades() override;

    void init();

    bool startFadeDrag(const ClipKeyList& clips, const ClipKey& leadClip, ClipSide side) override;
    bool changeFade(secs_t duration) override;
    bool commitFadeDrag() override;
    void cancelFadeDrag() override;
    std::optional<DraggedFade> draggedFade() const override;
    muse::async::Notification draggedFadeChanged() const override;
    FadeEnds snappableFadeEnds(const Clip& clipKey) const override;

private:
    void endFadeDrag();
    friend class ClipFadesTests;

    std::unique_ptr<ClipFadeDrag> m_drag;
    muse::async::Notification m_draggedFadeChanged;
};
}
