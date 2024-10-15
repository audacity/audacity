/*!********************************************************************
*
 Audacity: A Digital Audio Editor

 NoteTrackAffordanceControls.h

 Vitaly Sverchinsky

 **********************************************************************/

#pragma once

#include "../../../ui/CommonTrackPanelCell.h"

class AffordanceHandle;
class SelectHandle;
class NoteTrackAffordanceHandle;

class AUDACITY_DLL_API NoteTrackAffordanceControls : public CommonTrackCell
{
    std::weak_ptr<NoteTrackAffordanceHandle> mAffordanceHandle;
    std::weak_ptr<SelectHandle> mSelectHandle;
public:
    NoteTrackAffordanceControls(const std::shared_ptr<Track>& pTrack);

    std::vector<UIHandlePtr> HitTest(const TrackPanelMouseState& state, const AudacityProject* pProject) override;

    void Draw(TrackPanelDrawingContext& context, const wxRect& rect, unsigned iPass) override;

    bool IsSelected() const;
};
