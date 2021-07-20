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
    static HitTestPreview HitPreview(const AudacityProject*, bool unsafe, bool moving);
public:

    void Enter(bool forward, AudacityProject* pProject) override;
    HitTestPreview Preview(const TrackPanelMouseState& mouseState, AudacityProject* pProject) override;

    AffordanceHandle(const std::shared_ptr<Track>& track);

    Result Click(const TrackPanelMouseEvent& evt, AudacityProject* pProject) override;
    Result Release(const TrackPanelMouseEvent& event, AudacityProject* pProject, wxWindow* pParent) override;

protected:
    virtual Result SelectAt(const TrackPanelMouseEvent& event, AudacityProject* pProject) = 0;
};
