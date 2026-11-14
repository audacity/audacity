/*!
 @file LabelTrackShifter.cpp
 @brief headerless file injects method definitions for time shifting of LabelTrack
 */

#include "LabelTrackView.h"
#include "../../ui/TimeShiftHandle.h"
#include "LabelTrack.h"
#include "ViewInfo.h"

class LabelTrackShifter final : public TrackShifter
{
public:
    LabelTrackShifter(LabelTrack& track, AudacityProject& project)
        : mpTrack{track.SharedPointer<LabelTrack>()}
        , mProject{project}
    {
        InitIntervals();
        mSubscription = mpTrack->Subscribe([this](const LabelTrackEvent& e){
            switch (e.type) {
                case LabelTrackEvent::Permutation:
                    return OnLabelPermuted(e);
                case LabelTrackEvent::Addition:
                    return OnLabelAdded(e);
                case LabelTrackEvent::Deletion:
                    return OnLabelDeleted(e);
                default:
                    return;
            }
        });
    }

    ~LabelTrackShifter() override
    {
    }

    Track& GetTrack() const override { return *mpTrack; }

    static inline size_t& GetIndex(ChannelGroupInterval& interval)
    {
        return static_cast<LabelTrack::Interval&>(interval).index;
    }

    static inline size_t GetIndex(const ChannelGroupInterval& interval)
    {
        return GetIndex(const_cast<ChannelGroupInterval&>(interval));
    }

    HitTestResult HitTest(
        double time, const ViewInfo& viewInfo, HitTestParams* pParams) override
    {
        HitTestResult result = HitTestResult::Intervals;
        auto t0 = viewInfo.selectedRegion.t0();
        auto t1 = viewInfo.selectedRegion.t1();
        if (mpTrack->IsSelected() && time >= t0 && time < t1) {
            result = HitTestResult::Selection;
        }

        // Prefer the box that the mouse hovers over, else the selected one
        int iLabel = -1;
        if (pParams) {
            iLabel
                =LabelTrackView::OverATextBox(*mpTrack, pParams->xx, pParams->yy);
        }
        if (iLabel == -1) {
            iLabel = LabelTrackView::Get(*mpTrack).GetNavigationIndex(mProject);
        }
        if (iLabel != -1) {
            UnfixIntervals([&](const auto& myInterval){
                return GetIndex(myInterval) == iLabel;
            });
            return result;
        } else {
            // If the pick is within the selection, which overlaps some intervals,
            // then move those intervals only
            // Else move all labels (preserving the older beahvior of time shift)
            if (result == HitTestResult::Selection) {
                SelectInterval({ t0, t1 });
            }
            if (mMoving.empty()) {
                return HitTestResult::Track;
            } else {
                return result;
            }
        }
    }

    void SelectInterval(TimeInterval interval) override
    {
        CommonSelectInterval(interval);
    }

    bool SyncLocks() override { return false; }

    bool MayMigrateTo(Track& otherTrack) override
    {
        return CommonMayMigrateTo(otherTrack);
    }

    /* We need to copy a complete label when detaching it because LabelStruct
       is stored in a vector in LabelTrack without an extra indirection.
       So the detached intervals handed back to the caller are unlike those
       reported by LabelTrack, but carry the extra information. */
    struct MovingInterval final : ChannelGroupInterval {
        SelectedRegion region;
        wxString title;
        MovingInterval(double start, double end, const LabelStruct& label)
            : start{start}, end{end}
            , region{label.selectedRegion}
            , title{label.title}
        {}
        double Start() const override { return start; }
        double End() const override { return end; }
        const double start, end;
    };

    Intervals Detach() override
    {
        auto pTrack = mpTrack.get();
        auto moveLabel = [pTrack](const auto& pInterval) {
            auto index = GetIndex(*pInterval);
            auto result = std::make_shared<MovingInterval>(
                pInterval->Start(), pInterval->End(), *pTrack->GetLabel(index));
            pTrack->DeleteLabel(index);
            return result;
        };
        Intervals result;
        std::transform(
            // Reverse traversal may lessen the shifting-left in the label array
            // responding to label deletion messages published by DeleteLabel
            mMoving.rbegin(), mMoving.rend(), std::back_inserter(result),
            moveLabel);
        mMoving = Intervals{};
        return result;
    }

    bool AdjustFit(
        const Track&, const Intervals&, double&, double) override
    {
        // Labels have no overlapping constraints, so just...
        return true;
    }

    bool Attach(Intervals intervals, double offset) override
    {
        auto pTrack = mpTrack.get();
        std::for_each(intervals.rbegin(), intervals.rend(),
                      [this, pTrack, offset](const auto& pInterval){
            auto data = static_cast<const MovingInterval&>(*pInterval);
            if (offset != .0) {
                data.region.move(offset);
            }
            auto index = pTrack->AddLabel(data.region, data.title);
            // Recreate the simpler TrackInterval as would be reported by LabelTrack
            mMoving.emplace_back(pTrack->MakeInterval(index));
        });
        return true;
    }

    void DoHorizontalOffset(double offset) override
    {
        auto& labels = mpTrack->GetLabels();
        for (auto& pInterval : MovingIntervals()) {
            auto index = GetIndex(*pInterval);
            auto labelStruct = labels[index];
            labelStruct.selectedRegion.move(offset);
            mpTrack->SetLabel(index, labelStruct);
        }

        mpTrack->SortLabels(); // causes callback to OnLabelPermuted
    }

private:
    void OnLabelPermuted(const LabelTrackEvent& e)
    {
        if (e.mpTrack.lock() != mpTrack) {
            return;
        }

        auto former = e.mFormerPosition;
        auto present = e.mPresentPosition;

        // Avoid signed-unsigned comparison below!
        if (former < 0 || present < 0) {
            assert(false);
            return;
        }

        auto update = [=](const auto& pInterval){
            auto& index = GetIndex(*pInterval);
            if (index == former) {
                index = present;
            } else if (former < index && index <= present) {
                --index;
            } else if (former > index && index >= present) {
                ++index;
            }
        };

        std::for_each(mFixed.begin(), mFixed.end(), update);
        std::for_each(mMoving.begin(), mMoving.end(), update);
    }

    void OnLabelAdded(const LabelTrackEvent& e)
    {
        if (e.mpTrack.lock() != mpTrack) {
            return;
        }

        auto present = e.mPresentPosition;

        // Avoid signed-unsigned comparison below!
        if (present < 0) {
            assert(false);
            return;
        }

        auto update = [=](const auto& pInterval){
            auto& index = GetIndex(*pInterval);
            if (index >= present) {
                ++index;
            }
        };

        std::for_each(mFixed.begin(), mFixed.end(), update);
        std::for_each(mMoving.begin(), mMoving.end(), update);
    }

    void OnLabelDeleted(const LabelTrackEvent e)
    {
        if (e.mpTrack.lock() != mpTrack) {
            return;
        }

        auto former = e.mFormerPosition;

        // Avoid signed-unsigned comparison below!
        if (former < 0) {
            assert(false);
            return;
        }

        auto update = [=](const auto& pInterval){
            auto& index = GetIndex(*pInterval);
            if (index > former) {
                --index;
            } else if (index == former) {
                // It should have been deleted first!
                assert(false);
            }
        };

        std::for_each(mFixed.begin(), mFixed.end(), update);
        std::for_each(mMoving.begin(), mMoving.end(), update);
    }

    Observer::Subscription mSubscription;
    const std::shared_ptr<LabelTrack> mpTrack;
    AudacityProject& mProject;
};

using MakeLabelTrackShifter = MakeTrackShifter::Override<LabelTrack>;
DEFINE_ATTACHED_VIRTUAL_OVERRIDE(MakeLabelTrackShifter) {
    return [](LabelTrack& track, AudacityProject& project) {
        return std::make_unique<LabelTrackShifter>(track, project);
    };
}
