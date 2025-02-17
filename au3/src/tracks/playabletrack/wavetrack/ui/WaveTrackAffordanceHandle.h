/*!********************************************************************
*
 Audacity: A Digital Audio Editor

 @file WaveTrackAffordanceHandle.h

 Vitaly Sverchinsky

 **********************************************************************/

#pragma once

#include "../../../ui/AffordanceHandle.h"

class ClipTimes;

//! Implements some features which are specific to Wave Clips
class WaveTrackAffordanceHandle final : public AffordanceHandle
{
    std::shared_ptr<ClipTimes> mTarget;
public:
    WaveTrackAffordanceHandle(const std::shared_ptr<Track>& track, const std::shared_ptr<ClipTimes>& target);

    Result Click(const TrackPanelMouseEvent& event, AudacityProject* project) override;

    UIHandle::Result SelectAt(const TrackPanelMouseEvent& event, AudacityProject* project) override;

    bool HandlesRightClick() override;

    UIHandle::Result Release(const TrackPanelMouseEvent& event, AudacityProject* pProject, wxWindow* pParent) override;
};
