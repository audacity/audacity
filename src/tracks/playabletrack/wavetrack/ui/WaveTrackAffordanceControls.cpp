/*!********************************************************************
*
 Audacity: A Digital Audio Editor

 WaveTrackAffordanceControls.cpp

 Vitaly Sverchinsky

 **********************************************************************/

#include "WaveTrackAffordanceControls.h"

#include <wx/dc.h>
#include <wx/frame.h>

#include "../../../../AllThemeResources.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "../../../../TrackArtist.h"
#include "../../../../TrackPanelDrawingContext.h"
#include "../../../../TrackPanelResizeHandle.h"
#include "ViewInfo.h"
#include "../../../../WaveTrack.h"
#include "../../../../WaveClip.h"
#include "../../../../UndoManager.h"
#include "../../../../ShuttleGui.h"
#include "../../../../ProjectWindows.h"
#include "../../../../commands/AudacityCommand.h"
#include "../../../ui/AffordanceHandle.h"
#include "WaveTrackView.h"//need only ClipParameters

#include "../../../../ProjectHistory.h"
#include "../../../../SelectionState.h"
#include "../../../../RefreshCode.h"
#include "../../../../Theme.h"

class WaveTrackAffordanceHandle final : public AffordanceHandle
{
public:
    WaveTrackAffordanceHandle(const std::shared_ptr<Track>& track) : AffordanceHandle(track) { }

    UIHandle::Result SelectAt(const TrackPanelMouseEvent& event, AudacityProject* pProject) override
    {
        const auto track = std::dynamic_pointer_cast<WaveTrack>(TrackList::Get(*pProject).Lock<Track>(GetTrack()));

        auto& viewInfo = ViewInfo::Get(*pProject);

        auto time = viewInfo.PositionToTime(event.event.m_x, event.rect.x);
        
        WaveClip* const selectedClip = track->GetClipAtTime(time);
        if (selectedClip) {
            viewInfo.selectedRegion.setTimes(selectedClip->GetOffset(), selectedClip->GetEndTime());
        }

        ProjectHistory::Get(*pProject).ModifyState(false);

        return RefreshCode::RefreshAll | RefreshCode::Cancelled;
    }
};

class SetWaveClipNameCommand : public AudacityCommand
{
public:
    static const ComponentInterfaceSymbol Symbol;

    ComponentInterfaceSymbol GetSymbol() override
    {
        return Symbol;
    }
    void PopulateOrExchange(ShuttleGui& S) override
    {
        S.AddSpace(0, 5);

        S.StartMultiColumn(2, wxALIGN_CENTER);
        {
            S.TieTextBox(XXO("Name:"), mName, 60);
        }
        S.EndMultiColumn();
    }
public:
    wxString mName;
};

const ComponentInterfaceSymbol SetWaveClipNameCommand::Symbol
{ XO("Set Wave Clip Name") };

WaveTrackAffordanceControls::WaveTrackAffordanceControls(const std::shared_ptr<Track>& pTrack)
    : CommonTrackCell(pTrack), mClipNameFont(wxFont(wxFontInfo()))
{
}

std::vector<UIHandlePtr> WaveTrackAffordanceControls::HitTest(const TrackPanelMouseState& state, const AudacityProject* pProject)
{
    mFocusClip.reset();

    std::vector<UIHandlePtr> results;

    auto px = state.state.m_x;
    auto py = state.state.m_y;

    const auto rect = state.rect;

    const auto track = FindTrack();

    auto trackList = track->GetOwner();
    if ((std::abs(rect.GetTop() - py) <= WaveTrackView::kChannelSeparatorThickness / 2) 
        && trackList
        && !track->IsLeader())
    {
        //given that track is not a leader there always should be
        //another track before this one
        auto prev = --trackList->Find(track.get());
        results.push_back(
            AssignUIHandlePtr(
                mResizeHandle, 
                std::make_shared<TrackPanelResizeHandle>((*prev)->shared_from_this(), py)
            )
        );
    }

    const auto waveTrack = std::static_pointer_cast<WaveTrack>(track->SubstitutePendingChangedTrack());

    auto& zoomInfo = ViewInfo::Get(*pProject);
    for (const auto& clip : waveTrack->GetClips())
    {
        auto affordanceRect = ClipParameters::GetClipRect(*clip.get(), zoomInfo, rect);

        if (affordanceRect.Contains(px, py))
        {
            results.push_back(
                AssignUIHandlePtr(
                    mAffordanceHandle,
                    std::make_shared<WaveTrackAffordanceHandle>(track)
                )
            );
            mFocusClip = clip;
            break;
        }
    }

    return results;
}

void WaveTrackAffordanceControls::Draw(TrackPanelDrawingContext& context, const wxRect& rect, unsigned iPass)
{
    if (iPass == TrackArtist::PassBackground) {
        auto track = FindTrack();
        const auto artist = TrackArtist::Get(context);

        TrackArt::DrawBackgroundWithSelection(context, rect, track.get(), artist->blankSelectedBrush, artist->blankBrush);

        const auto waveTrack = std::static_pointer_cast<WaveTrack>(track->SubstitutePendingChangedTrack());
        const auto& zoomInfo = *artist->pZoomInfo;

        context.dc.SetClippingRegion(rect);

        context.dc.SetTextBackground(wxTransparentColor);
        context.dc.SetTextForeground(theTheme.Colour(clrClipNameText));
        context.dc.SetFont(mClipNameFont);

        auto px = context.lastState.m_x;
        auto py = context.lastState.m_y;

        for (const auto& clip : waveTrack->GetClips())
        {
            auto affordanceRect 
                = ClipParameters::GetClipRect(*clip.get(), zoomInfo, rect);
            if (affordanceRect.IsEmpty())
                continue;
            
            auto selected = GetSelectedClip().lock() == clip;
            auto highlight = selected || affordanceRect.Contains(px, py);
            TrackArt::DrawClipAffordance(context.dc, affordanceRect, clip->GetName(), highlight, selected);

        }
        context.dc.DestroyClippingRegion();
    }
}

bool WaveTrackAffordanceControls::StartEditClipName(AudacityProject* project)
{
    if (auto lock = mFocusClip.lock())
    {
        auto clip = lock.get();

        SetWaveClipNameCommand Command;
        auto oldName = clip->GetName();
        Command.mName = oldName;
        auto result = Command.PromptUser(&GetProjectFrame(*project));
        if (result && Command.mName != oldName)
        {
            clip->SetName(Command.mName);
            ProjectHistory::Get(*project).PushState(XO("Modified Clip Name"),
                XO("Clip Name Edit"), UndoPush::CONSOLIDATE);

            return true;
        }
    }
    return false;
}

std::weak_ptr<WaveClip> WaveTrackAffordanceControls::GetSelectedClip() const
{
    if (auto handle = mAffordanceHandle.lock())
    {
        return handle->Clicked() ? mFocusClip : std::weak_ptr<WaveClip>();
    }
    return {};
}

unsigned WaveTrackAffordanceControls::CaptureKey(wxKeyEvent& event, ViewInfo& viewInfo, wxWindow* pParent, AudacityProject* project)
{
    const auto keyCode = event.GetKeyCode();
    if (!(keyCode == WXK_RETURN || keyCode == WXK_NUMPAD_ENTER || keyCode == WXK_TAB))
        event.Skip();
    return RefreshCode::RefreshNone;
}

unsigned WaveTrackAffordanceControls::KeyDown(wxKeyEvent& event, ViewInfo& viewInfo, wxWindow*, AudacityProject* project)
{
    auto keyCode = event.GetKeyCode();
    switch (keyCode)
    {
    case WXK_TAB:
        SelectNextClip(viewInfo, project, event.GetModifiers() != wxMOD_SHIFT);
        break;
    case WXK_NUMPAD_ENTER:
    case WXK_RETURN:
        StartEditSelectedClipName(viewInfo, project);
        break;
    }
    return RefreshCode::RefreshCell;
}

namespace {
    template<typename Iter, typename Comp>
    WaveClip* NextClipLooped(ViewInfo& viewInfo, Iter begin, Iter end, Comp comp)
    {
        auto it = std::find_if(begin, end, [&](WaveClip* clip) {
            return clip->GetStartTime() == viewInfo.selectedRegion.t0() &&
                clip->GetEndTime() == viewInfo.selectedRegion.t1();
        });
        if (it == end)
            it = std::find_if(begin, end, comp);
        else
            it = std::next(it);
        
        if (it == end)
            return *begin;
        return *it;
    }
}


bool WaveTrackAffordanceControls::SelectNextClip(ViewInfo& viewInfo, AudacityProject* project, bool forward)
{
    //Iterates through clips in a looped manner
    auto waveTrack = std::dynamic_pointer_cast<WaveTrack>(FindTrack());
    if (!waveTrack)
        return false;
    auto clips = waveTrack->SortedClipArray();
    if (clips.empty())
        return false;

    WaveClip* clip{ };
    if (forward)
    {
        clip = NextClipLooped(viewInfo, clips.begin(), clips.end(), [&](const WaveClip* other) {
            return other->GetStartTime() >= viewInfo.selectedRegion.t1();
        });
    }
    else
    {
        clip = NextClipLooped(viewInfo, clips.rbegin(), clips.rend(), [&](const WaveClip* other) {
            return other->GetStartTime() <= viewInfo.selectedRegion.t0();
        });
    }

    viewInfo.selectedRegion.setTimes(clip->GetOffset(), clip->GetEndTime());
    ProjectHistory::Get(*project).ModifyState(false);
    return true;
}

bool WaveTrackAffordanceControls::StartEditSelectedClipName(ViewInfo& viewInfo, AudacityProject* project)
{
    //Attempts to invoke name editing if there is a selected clip
    auto waveTrack = std::dynamic_pointer_cast<WaveTrack>(FindTrack());
    if (!waveTrack)
        return false;
    auto clips = waveTrack->GetClips();

    auto it = std::find_if(clips.begin(), clips.end(), [&](const std::shared_ptr<WaveClip>& clip) {
        return clip->GetStartTime() == viewInfo.selectedRegion.t0() &&
            clip->GetEndTime() == viewInfo.selectedRegion.t1();
        });
    if (it != clips.end())
    {
        mFocusClip = *it;
        return StartEditClipName(project);
    }
    return false;
}
