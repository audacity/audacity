/*!********************************************************************
*
 Audacity: A Digital Audio Editor

 WaveTrackAffordanceControls.h

 Vitaly Sverchinsky

 **********************************************************************/

#pragma once

#include <wx/font.h>

#include "Channel.h"
#include "Observer.h"
#include "ViewInfo.h"
#include "WaveTrack.h"
#include "tracks/ui/CommonTrackPanelCell.h"
#include "tracks/ui/TextEditHelper.h"

struct TrackListEvent;

class AffordanceHandle;
class SelectHandle;
class WaveClip;
class TrackPanelResizeHandle;
class WaveClipTitleEditHandle;
class ClipOverflowButtonHandle;
class ClipPitchAndSpeedButtonHandle;
class WaveTrackAffordanceHandle;
class WaveClipAdjustBorderHandle;
class TrackList;

//Handles clip movement, selection, navigation and
//allow name change
class AUDACITY_DLL_API WaveTrackAffordanceControls : public CommonTrackCell, public TextEditDelegate,
    public std::enable_shared_from_this<WaveTrackAffordanceControls>
{
    using IntervalIterator = ChannelGroup::IntervalIterator<WaveTrack::Interval>;

    IntervalIterator mFocusInterval;
    std::weak_ptr<ClipOverflowButtonHandle> mOverflowButtonHandle;
    std::weak_ptr<ClipPitchAndSpeedButtonHandle> mPitchButtonHandle;
    std::weak_ptr<ClipPitchAndSpeedButtonHandle> mSpeedButtonHandle;
    std::weak_ptr<WaveTrackAffordanceHandle> mAffordanceHandle;
    std::weak_ptr<TrackPanelResizeHandle> mResizeHandle;
    std::weak_ptr<WaveClipTitleEditHandle> mTitleEditHandle;
    std::weak_ptr<SelectHandle> mSelectHandle;
    std::weak_ptr<WaveClipAdjustBorderHandle> mClipBorderAdjustHandle;

    IntervalIterator mEditedInterval;
    std::shared_ptr<TextEditHelper> mTextEditHelper;

    wxFont mClipNameFont;

    //Helper container used to track clips names visibility
    std::vector<IntervalIterator> mVisibleIntervals;

public:
    WaveTrackAffordanceControls(const std::shared_ptr<Track>& pTrack);

    std::vector<UIHandlePtr> HitTest(const TrackPanelMouseState& state, const AudacityProject* pProject) override;

    void Draw(TrackPanelDrawingContext& context, const wxRect& rect, unsigned iPass) override;

    IntervalIterator GetSelectedInterval() const;

    unsigned CaptureKey(wxKeyEvent& event, ViewInfo& viewInfo, wxWindow* pParent, AudacityProject* project) override;

    unsigned KeyDown(wxKeyEvent& event, ViewInfo& viewInfo, wxWindow* pParent, AudacityProject* project) override;

    unsigned Char(wxKeyEvent& event, ViewInfo& viewInfo, wxWindow* pParent, AudacityProject* project) override;

    unsigned LoseFocus(AudacityProject* project) override;

    void OnTextEditFinished(AudacityProject* project, const wxString& text) override;
    void OnTextEditCancelled(AudacityProject* project) override;
    void OnTextModified(AudacityProject* project, const wxString& text) override;
    void OnTextContextMenu(AudacityProject* project, const wxPoint& position) override;

    unsigned OnAffordanceClick(const TrackPanelMouseEvent& event, AudacityProject* project);

    bool OnTextCopy(AudacityProject& project);
    bool OnTextCut(AudacityProject& project);
    bool OnTextPaste(AudacityProject& project);
    bool OnTextSelect(AudacityProject& project);

    void StartEditSelectedClipName(AudacityProject& project);

    void StartEditSelectedClipSpeed(AudacityProject& project);

    void OnRenderClipStretching(AudacityProject& project) const;

    std::vector<MenuItem> GetMenuItems(
        const wxRect& rect, const wxPoint* pPosition, AudacityProject* pProject)
    override;
private:

    bool IsIntervalVisible(const IntervalIterator& it) const noexcept;
    ///@brief Starts in-place clip name editing or shows a Clip Name Edit dialog, depending on prefs
    ///@param clip to be edited. Should belong to the same `WaveTrack` as returned by `FindTrack()`
    bool StartEditClipName(AudacityProject& project, IntervalIterator it);

    void ResetClipNameEdit();

    void OnTrackListEvent(const TrackListEvent& evt);

    void OnSelectionChange(NotifyingSelectedRegionMessage);

    unsigned ExitTextEditing();

    std::shared_ptr<TextEditHelper> MakeTextEditHelper(const wxString& text);

    Observer::Subscription mTrackListEventSubscription;
    Observer::Subscription mSelectionChangeSubscription;
};
