/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "../../iselectioncontroller.h"

#include "au3wrap/au3types.h"

#include "../../ilabelsinteraction.h"

namespace au::trackedit {
class Au3LabelsInteraction : public ILabelsInteraction
{
    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<ISelectionController> selectionController;

public:
    Au3LabelsInteraction();

    bool addLabelToSelection() override;
    bool changeLabelTitle(const LabelKey& labelKey, const muse::String& title) override;

    bool removeLabel(const LabelKey& labelKey) override;
    bool removeLabels(const LabelKeyList& labelKeys) override;

    ITrackDataPtr cutLabel(const LabelKey& labelKey) override;
    ITrackDataPtr copyLabel(const LabelKey& labelKey) override;

    bool moveLabels(secs_t timePositionOffset, bool completed) override;

    bool stretchLabelLeft(const LabelKey& labelKey, secs_t newStartTime, bool completed) override;
    bool stretchLabelRight(const LabelKey& labelKey, secs_t newEndTime, bool completed) override;

    muse::Progress progress() const override;

private:
    friend class Au3LabelsInteractionsTests;

    au3::Au3Project& projectRef() const;

    context::IPlaybackStatePtr playbackState() const;

    std::optional<secs_t> getLeftmostLabelStartTime(const LabelKeyList& labelKeys) const;

    muse::Progress m_progress;
    std::atomic<bool> m_busy = false;

    std::optional<secs_t> m_stretchTime;
    std::optional<LabelKey> m_stretchingLabelKey;
};
}
