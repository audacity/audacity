/*!********************************************************************
*
 Audacity: A Digital Audio Editor

 @file WaveTrackAffordanceHandle.cpp

 Vitaly Sverchinsky

 **********************************************************************/

#include "WaveTrackAffordanceHandle.h"
#include "WaveTrackAffordanceControls.h"
#include "WaveChannelView.h"
#include "ViewInfo.h"

#include "../../../../RefreshCode.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "PitchAndSpeedDialog.h"
#include "ProjectHistory.h"
#include "WaveClip.h"

#include <wx/event.h>

WaveTrackAffordanceHandle::WaveTrackAffordanceHandle(const std::shared_ptr<Track>& track, const std::shared_ptr<ClipTimes>& target)
    : AffordanceHandle(track), mTarget(target)
{ }

UIHandle::Result WaveTrackAffordanceHandle::Click(const TrackPanelMouseEvent& event, AudacityProject* project)
{
    // We only care about left clicks here,
    // however we need to intercept Release events for right clicks later
    if (event.event.GetButton() != wxMOUSE_BTN_LEFT) {
        return RefreshCode::RefreshNone;
    }

    Result result = RefreshCode::RefreshNone;

    if (WaveChannelView::ClipDetailsVisible(*mTarget, ViewInfo::Get(*project), event.rect)) {
        auto affordanceControl = std::dynamic_pointer_cast<WaveTrackAffordanceControls>(event.pCell);

        if (affordanceControl) {
            result |= affordanceControl->OnAffordanceClick(event, project);
            if (!event.event.GetSkipped()) {//event is "consumed"
                return result | RefreshCode::Cancelled;
            }
            event.event.Skip(false);
        }
    }
    return result | AffordanceHandle::Click(event, project);
}

UIHandle::Result WaveTrackAffordanceHandle::SelectAt(const TrackPanelMouseEvent& event, AudacityProject* project)
{
    auto& viewInfo = ViewInfo::Get(*project);
    viewInfo.selectedRegion.setTimes(mTarget->GetPlayStartTime(), mTarget->GetPlayEndTime());

    ProjectHistory::Get(*project).ModifyState(false);

    return RefreshCode::RefreshAll | RefreshCode::Cancelled;
}

bool WaveTrackAffordanceHandle::HandlesRightClick()
{
    return true;
}

UIHandle::Result WaveTrackAffordanceHandle::Release(const TrackPanelMouseEvent& event, AudacityProject* pProject, wxWindow* pParent)
{
    auto result = AffordanceHandle::Release(event, pProject, pParent);
    PitchAndSpeedDialog::Get(*pProject).TryRetarget(event);

    if (event.event.RightUp()) {
        result |= event.pCell->DoContextMenu(event.rect, pParent, nullptr, pProject);
    }

    return result;
}
