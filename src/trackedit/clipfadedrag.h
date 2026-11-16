/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <memory>
#include <optional>

#include "trackedit/dom/clip.h"
#include "trackedit/iprojecthistory.h"
#include "trackedit/itrackeditinteraction.h"
#include "trackedit/itrackeditproject.h"

namespace au::trackedit {
class ClipFadeDrag
{
public:
    //! Null if the lead clip is not among the clips, or if a user interaction is already ongoing, e.g. that of a previous drag.
    //! The user interaction lasts as long as the returned drag. Destroyed without a `commit()`, the drag gives the clips their fades back.
    static std::unique_ptr<ClipFadeDrag> start(ITrackeditInteraction& interaction, IProjectHistory& history, ITrackeditProjectPtr project,
                                               Clips clips, const ClipKey& leadClip, ClipSide side);
    ~ClipFadeDrag();

    ClipFadeDrag(const ClipFadeDrag&) = delete;
    ClipFadeDrag& operator=(const ClipFadeDrag&) = delete;

    const ClipKey& leadClip() const;
    ClipSide side() const;
    bool changeFade(double duration);

    // Drag gets canceled if destroyed without commit()
    bool commit();

private:
    ClipFadeDrag(ITrackeditInteraction& interaction, IProjectHistory& history, ITrackeditProjectPtr project, Clips clips,
                 const ClipKey& leadClip, ClipSide side);

    double fadeLength(const Clip& clip) const;
    double leadFadeLength(double duration) const;
    bool changeClip(const ClipKey& clip, double length, bool completed);
    void rollback();

    ITrackeditInteraction& m_interaction;
    IProjectHistory& m_history;
    const ITrackeditProjectPtr m_project;
    const Clips m_clips;
    const ClipSide m_side;
    //! The lead clip's fade length after the last change, which `commit()` makes again, completed this time
    std::optional<double> m_leadFadeLength;
    bool m_committed = false;
};
}
