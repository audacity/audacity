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
    if (!m_drag) {
        return false;
    }
    m_draggedFadeChanged.notify();
    return true;
}

bool ClipFades::changeFade(secs_t duration)
{
    if (m_drag && m_drag->changeFade(duration)) {
        m_draggedFadeChanged.notify();
        return true;
    }
    return false;
}

void ClipFades::endFadeDrag()
{
    // Rolls back unless committed; nulls m_drag before the rollback, see cancelFadeDrag
    m_drag.reset();
    m_draggedFadeChanged.notify();
}

bool ClipFades::commitFadeDrag()
{
    if (!m_drag) {
        return false;
    }
    const bool committed = m_drag->commit();
    endFadeDrag();
    return committed;
}

void ClipFades::cancelFadeDrag()
{
    if (m_drag) {
        endFadeDrag();
    }
}

std::optional<IClipFades::DraggedFade> ClipFades::draggedFade() const
{
    if (!m_drag) {
        return std::nullopt;
    }
    return DraggedFade { m_drag->leadClip(), m_drag->side() };
}

muse::async::Notification ClipFades::draggedFadeChanged() const
{
    return m_draggedFadeChanged;
}

IClipFades::FadeEnds ClipFades::snappableFadeEnds(const Clip& clip) const
{
    if (!clip.isValid()) {
        LOGW() << "Invalid clip";
        return {};
    }
    if (m_drag) {
        return m_drag->snappableFadeEnds(clip);
    }
    return { clip.startTime + clip.fadeInDuration, clip.endTime - clip.fadeOutDuration };
}
}
