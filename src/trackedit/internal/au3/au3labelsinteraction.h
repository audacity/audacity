/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "../../iprojecthistory.h"
#include "../../iselectioncontroller.h"

#include "au3wrap/au3types.h"

#include "../../ilabelsinteraction.h"

namespace au::trackedit {
class Au3LabelsInteraction : public ILabelsInteraction
{
    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<ISelectionController> selectionController;
    muse::Inject<IProjectHistory> projectHistory;

public:
    Au3LabelsInteraction();

    bool addLabelToSelection() override;
    bool changeLabelTitle(const LabelKey& labelKey, const muse::String& title) override;

    bool moveLabels(secs_t timePositionOffset, bool completed) override;

    muse::Progress progress() const override;

private:
    friend class Au3LabelsInteractionsTests;

    au3::Au3Project& projectRef() const;

    context::IPlaybackStatePtr playbackState() const;

    muse::Progress m_progress;
    std::atomic<bool> m_busy = false;
};
}
