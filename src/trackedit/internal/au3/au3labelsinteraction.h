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
class Au3LabelsInteraction : public ILabelsInteraction, public muse::Injectable
{
    muse::Inject<context::IGlobalContext> globalContext{ this };
    muse::Inject<ISelectionController> selectionController{ this };

public:
    Au3LabelsInteraction(const muse::modularity::ContextPtr& ctx);

    muse::RetVal<LabelKey> addLabel(const TrackId& toTrackId) override;
    bool addLabelToSelection() override;

    bool changeLabelTitle(const LabelKey& labelKey, const muse::String& title) override;
    bool changeLabelLowFrequency(const LabelKey& labelKey, double frequency) override;
    bool changeLabelHighFrequency(const LabelKey& labelKey, double frequency) override;

    bool removeLabel(const LabelKey& labelKey) override;
    bool removeLabels(const LabelKeyList& labelKeys, bool moveLabels) override;

    ITrackDataPtr cutLabel(const LabelKey& labelKey) override;

    ITrackDataPtr copyLabel(const LabelKey& labelKey) override;

    bool moveLabels(const LabelKeyList& labelKeys, secs_t timePositionOffset) override;
    muse::RetVal<LabelKeyList> moveLabelsToTrack(const LabelKeyList& labelKeys, const trackedit::TrackId& toTrackId) override;

    bool stretchLabelLeft(const LabelKey& labelKey, secs_t newStartTime, bool completed) override;
    bool stretchLabelRight(const LabelKey& labelKey, secs_t newEndTime, bool completed) override;

    std::optional<secs_t> getLeftmostLabelStartTime(const LabelKeyList& labelKeys) const override;

    muse::Progress progress() const override;

private:
    friend class Au3LabelsInteractionsTests;

    au3::Au3Project& projectRef() const;

    context::IPlaybackStatePtr playbackState() const;

    muse::Progress m_progress;
    std::atomic<bool> m_busy = false;

    std::optional<secs_t> m_stretchTime;
    std::optional<LabelKey> m_stretchingLabelKey;
};
}
