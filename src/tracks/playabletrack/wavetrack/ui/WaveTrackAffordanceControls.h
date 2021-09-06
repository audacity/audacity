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

    //Invokes name editing for a clip that currently is
    //in focus(as a result of hit testing), returns true on success
    //false if there is no focus
    bool StartEditClipName(AudacityProject* project);

    std::weak_ptr<WaveClip> GetSelectedClip() const;
    unsigned CaptureKey
    (wxKeyEvent& event, ViewInfo& viewInfo, wxWindow* pParent,
        AudacityProject* project) override;
    
    unsigned KeyDown (wxKeyEvent& event, ViewInfo& viewInfo, wxWindow* pParent,
        AudacityProject* project) override;

private:
    bool StartEditSelectedClipName(ViewInfo& viewInfo, AudacityProject* project);
};
