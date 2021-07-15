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
 
    static UIHandlePtr HitAnywhere(std::weak_ptr<AffordanceHandle>& holder, const std::shared_ptr<Track>& pTrack);

    HitTestPreview Preview(const TrackPanelMouseState& mouseState, AudacityProject* pProject) override;

    AffordanceHandle(const std::shared_ptr<Track>& track);

    Result Click(const TrackPanelMouseEvent& evt, AudacityProject* pProject) override;
    Result Release(const TrackPanelMouseEvent& event, AudacityProject* pProject, wxWindow* pParent) override;
};
