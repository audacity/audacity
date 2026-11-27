/*
 * Audacity: A Digital Audio Editor
 */
#include "clipfadedrag.h"

#include <algorithm>
#include <cassert>

#include "global/realfn.h"
#include "log.h"

namespace au::trackedit {
namespace {
Clips withLeadClipLast(Clips clips, const ClipKey& leadClip)
{
    std::stable_partition(clips.begin(), clips.end(), [&](const Clip& clip) {
        return clip.key != leadClip;
    });
    return clips;
}
}

std::unique_ptr<ClipFadeDrag> ClipFadeDrag::start(ITrackeditInteraction& interaction, IProjectHistory& history,
                                                  ITrackeditProjectPtr project, Clips clips, const ClipKey& leadClip, ClipSide side)
{
    if (std::none_of(clips.begin(), clips.end(), [&](const Clip& clip) { return clip.key == leadClip; })) {
        return nullptr;
    }
    if (history.interactionOngoing()) {
        LOGW() << "a user interaction is already ongoing";
        return nullptr;
    }
    return std::unique_ptr<ClipFadeDrag>(new ClipFadeDrag(interaction, history, std::move(project), std::move(clips), leadClip, side));
}

ClipFadeDrag::ClipFadeDrag(ITrackeditInteraction& interaction, IProjectHistory& history, ITrackeditProjectPtr project, Clips clips,
                           const ClipKey& leadClip, ClipSide side)
    : m_interaction(interaction), m_history(history), m_project(std::move(project)), m_clips(withLeadClipLast(std::move(clips), leadClip)),
    m_side(side)
{
    m_history.startUserInteraction();
}

ClipFadeDrag::~ClipFadeDrag()
{
    if (m_leadFadeLength && !m_committed) {
        rollback();
    }
    m_history.endUserInteraction();
}

const au::trackedit::ClipKey& ClipFadeDrag::leadClip() const
{
    return m_clips.back().key;
}

ClipSide ClipFadeDrag::side() const
{
    return m_side;
}

IClipFades::FadeEnds ClipFadeDrag::snappableFadeEnds(const Clip& clip) const
{
    IClipFades::FadeEnds ends { clip.startTime + clip.fadeInDuration, clip.endTime - clip.fadeOutDuration };
    const auto atStart = std::find_if(m_clips.begin(), m_clips.end(), [&](const Clip& c) { return c.key == clip.key; });
    if (atStart == m_clips.end()) {
        // `clip`'s fades aren't being dragged - both its fades are snappable.
        return ends;
    }

    // `clip`'s dragged fade (left or right) isn't snappable.
    const bool fadeInDragged = m_side == ClipSide::Left;
    (fadeInDragged ? ends.first : ends.second).reset();

    // `clip`'s non-dragged fade might still be pushed by the dragged one. If it is, it's not snappable anymore.
    const double otherLength = fadeInDragged ? clip.fadeOutDuration : clip.fadeInDuration;
    const double otherLengthAtStart = fadeInDragged ? atStart->fadeOutDuration : atStart->fadeInDuration;
    if (!muse::RealIsEqual(otherLength, otherLengthAtStart)) {
        (fadeInDragged ? ends.second : ends.first).reset();
    }

    return ends;
}

double ClipFadeDrag::fadeLength(const Clip& clip) const
{
    return m_side == ClipSide::Left ? clip.fadeInDuration : clip.fadeOutDuration;
}

double ClipFadeDrag::leadFadeLength(double duration) const
{
    // What the lead clip's fade becomes: the followers mustn't move further than it, e.g. when it is dragged below zero
    const Clip& lead = m_clips.back();
    return std::clamp(duration, 0.0, std::max(lead.endTime - lead.startTime, 0.0));
}

bool ClipFadeDrag::changeFade(double duration)
{
    const double leadLength = leadFadeLength(duration);
    const double offset = leadLength - fadeLength(m_clips.back());
    constexpr auto completed = false;
    bool changed = false;
    for (const Clip& clip : m_clips) {
        changed = changeClip(clip.key, std::max(fadeLength(clip) + offset, 0.0), completed);
    }
    m_leadFadeLength = leadLength;
    // That of the lead clip, the last one
    return changed;
}

bool ClipFadeDrag::commit()
{
    assert(!m_committed);
    if (!m_leadFadeLength || m_committed) {
        return false;
    }
    if (muse::RealIsEqual(*m_leadFadeLength, fadeLength(m_clips.back()))) {
        // E.g. a click without a drag, which must not make an undo step
        return false;
    }
    // Only one completed change, so that one state is pushed for all the clips
    constexpr auto completed = true;
    m_committed = changeClip(leadClip(), *m_leadFadeLength, completed);
    return m_committed;
}

bool ClipFadeDrag::changeClip(const ClipKey& clip, double length, bool completed)
{
    return m_side == ClipSide::Left ? m_interaction.changeClipFadeIn(clip, length, completed)
           : m_interaction.changeClipFadeOut(clip, length, completed);
}

void ClipFadeDrag::rollback()
{
    // To the state `startUserInteraction()` recorded, which has the fades as stored, e.g. beyond a trimmed clip's end,
    // whereas the clips only tell what takes effect
    m_history.rollbackState();
    m_project->reload();
}
}
