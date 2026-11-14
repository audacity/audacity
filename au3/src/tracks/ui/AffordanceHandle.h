/*!********************************************************************
*
 Audacity: A Digital Audio Editor

 AffordanceHandle.h

 Vitaly Sverchinsky

 **********************************************************************/

#pragma once

#include "TimeShiftHandle.h"

class AUDACITY_DLL_API AffordanceHandle : public TimeShiftHandle
{
    constexpr static double MoveThreshold { 5.0 };

    static HitTestPreview HitPreview(const AudacityProject*, bool unsafe, bool moving);

    bool mMoving { false };
    wxPoint mClickPosition { };
public:

    void Enter(bool forward, AudacityProject* pProject) override;
    HitTestPreview Preview(const TrackPanelMouseState& mouseState, AudacityProject* pProject) override;

    AffordanceHandle(const std::shared_ptr<Track>& track);

    Result Click(const TrackPanelMouseEvent& evt, AudacityProject* pProject) override;
    Result Drag(const TrackPanelMouseEvent& event, AudacityProject* pProject) override;
    Result Release(const TrackPanelMouseEvent& event, AudacityProject* pProject, wxWindow* pParent) override;

protected:
    virtual Result SelectAt(const TrackPanelMouseEvent& event, AudacityProject* pProject) = 0;

private:
    Result UpdateTrackSelection(
        const TrackPanelMouseEvent& event, AudacityProject* pProject);
};
