/*
 * Audacity: A Digital Audio Editor
 */
#include "clipfades.h"

#include <algorithm>
#include <iterator>

#include "clipfadedrag.h"

namespace au::trackedit {
ClipFades::ClipFades(const muse::modularity::ContextPtr& ctx)
    : muse::Contextable(ctx)
{
}

ClipFades::~ClipFades() = default;

void ClipFades::init()
{
    // Escape, or the project closing
    trackeditInteraction()->cancelDragEditRequested().onNotify(this, [this] {
        cancelFadeDrag();
    });
}

bool ClipFades::startFadeDrag(const ClipKeyList& clips, const ClipKey& leadClip, ClipSide side)
{
    const ITrackeditProjectPtr project = globalContext()->currentTrackeditProject();
    if (!project || !project->clip(leadClip).isValid()) {
        return false;
    }

    // The previous drag, if any, gives the clips their fades back and ends its user interaction before this one reads
    // the clips and starts its own
    cancelFadeDrag();
    Clips clipsAtStart;
    std::ranges::transform(clips, std::back_inserter(clipsAtStart), [&](const ClipKey& key) { return project->clip(key); });
    m_drag = ClipFadeDrag::start(*trackeditInteraction(), *projectHistory(), project, std::move(clipsAtStart), leadClip, side);
    return m_drag != nullptr;
}

bool ClipFades::changeFade(secs_t duration)
{
    return m_drag && m_drag->changeFade(duration);
}

bool ClipFades::commitFadeDrag()
{
    if (!m_drag) {
        return false;
    }
    const bool committed = m_drag->commit();
    cancelFadeDrag();
    return committed;
}

void ClipFades::cancelFadeDrag()
{
    m_drag.reset();
}

std::optional<IClipFades::DraggedFade> ClipFades::draggedFade() const
{
    if (!m_drag) {
        return std::nullopt;
    }
    return DraggedFade { m_drag->leadClip(), m_drag->side() };
}
}
