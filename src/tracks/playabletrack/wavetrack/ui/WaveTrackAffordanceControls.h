/*!********************************************************************
*
 Audacity: A Digital Audio Editor

 WaveTrackAffordanceControls.h

 Vitaly Sverchinsky

 **********************************************************************/

#pragma once

#include <wx/font.h>
#include "../../../ui/CommonTrackPanelCell.h"

class AffordanceHandle;
class WaveClip;
class TrackPanelResizeHandle;
class WaveTrackAffordanceHandle;

class AUDACITY_DLL_API WaveTrackAffordanceControls : public CommonTrackCell
{
    std::weak_ptr<WaveClip> mFocusClip;
    std::weak_ptr<WaveTrackAffordanceHandle> mAffordanceHandle;
    std::weak_ptr<TrackPanelResizeHandle> mResizeHandle;
    wxFont mClipNameFont;

public:
    WaveTrackAffordanceControls(const std::shared_ptr<Track>& pTrack);

    std::vector<UIHandlePtr> HitTest(const TrackPanelMouseState& state, const AudacityProject* pProject) override;

    void Draw(TrackPanelDrawingContext& context, const wxRect& rect, unsigned iPass) override;

    std::weak_ptr<WaveClip> GetSelectedClip() const;
};
