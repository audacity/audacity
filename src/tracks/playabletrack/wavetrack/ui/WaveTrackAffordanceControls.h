/*!********************************************************************
*
 Audacity: A Digital Audio Editor

 WaveTrackAffordanceControls.h

 Vitaly Sverchinsky

 **********************************************************************/

#pragma once

#include <wx/font.h>

#include "Observer.h"
#include "../../../ui/CommonTrackPanelCell.h"
#include "../../../ui/TextEditHelper.h"


struct TrackListEvent;

class AffordanceHandle;
class SelectHandle;
class WaveClip;
class TrackPanelResizeHandle;
class WaveClipTitleEditHandle;
class WaveTrackAffordanceHandle;
class WaveClipTrimHandle;
class TrackList;

//Handles clip movement, selection, navigation and
//allow name change
class AUDACITY_DLL_API WaveTrackAffordanceControls : 
    public CommonTrackCell,
    public TextEditDelegate,
    public std::enable_shared_from_this<WaveTrackAffordanceControls>
{
    std::weak_ptr<WaveClip> mFocusClip;
    std::weak_ptr<WaveTrackAffordanceHandle> mAffordanceHandle;
    std::weak_ptr<TrackPanelResizeHandle> mResizeHandle;
    std::weak_ptr<WaveClipTitleEditHandle> mTitleEditHandle;
    std::weak_ptr<SelectHandle> mSelectHandle;
    std::weak_ptr<WaveClipTrimHandle> mClipTrimHandle;

    std::weak_ptr<WaveClip> mEditedClip;
    std::shared_ptr<TextEditHelper> mTextEditHelper;

    wxFont mClipNameFont;

    //Helper flag, checked when text editing is triggered (and dialog-edit option is disabled)
    bool mClipNameVisible { false };

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

    unsigned Char
    (wxKeyEvent& event, ViewInfo& viewInfo, wxWindow* pParent,
        AudacityProject* project) override;

    unsigned LoseFocus(AudacityProject *project) override;

    void OnTextEditFinished(AudacityProject* project, const wxString& text) override;
    void OnTextEditCancelled(AudacityProject* project) override;
    void OnTextModified(AudacityProject* project, const wxString& text) override;
    void OnTextContextMenu(AudacityProject* project, const wxPoint& position) override;

    bool StartEditNameOfMatchingClip( AudacityProject &project,
        std::function<bool(WaveClip&)> test /*!<
            Edit the first clip in the track's list satisfying the test */
    );

    unsigned OnAffordanceClick(const TrackPanelMouseEvent& event, AudacityProject* project);

    bool OnTextCopy(AudacityProject& project);
    bool OnTextCut(AudacityProject& project);
    bool OnTextPaste(AudacityProject& project);
    bool OnTextSelect(AudacityProject& project);

private:
    void ResetClipNameEdit();

    void OnTrackChanged(const TrackListEvent& evt);

    unsigned ExitTextEditing();

    std::shared_ptr<TextEditHelper> MakeTextEditHelper(const wxString& text);

    Observer::Subscription mSubscription;
};
