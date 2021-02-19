/*!********************************************************************
*
 Audacity: A Digital Audio Editor

 WaveTrackAffordanceControls.cpp

 Vitaly Sverchinsky

 **********************************************************************/

#include "WaveTrackAffordanceControls.h"

#include <wx/dc.h>
#include <wx/frame.h>

#include "AllThemeResources.h"
#include "../../../../commands/CommandContext.h"
#include "../../../../commands/CommandFlag.h"
#include "../../../../commands/CommandFunctors.h"
#include "../../../../commands/CommandManager.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "../../../../TrackArt.h"
#include "../../../../TrackArtist.h"
#include "../../../../TrackPanelDrawingContext.h"
#include "../../../../TrackPanelResizeHandle.h"
#include "ViewInfo.h"
#include "WaveTrack.h"
#include "WaveClip.h"
#include "UndoManager.h"
#include "ShuttleGui.h"
#include "../../../../ProjectWindows.h"
#include "../../../../commands/AudacityCommand.h"

#include "../../../ui/TextEditHelper.h"
#include "../../../ui/SelectHandle.h"
#include "WaveTrackView.h"//need only ClipParameters
#include "WaveTrackAffordanceHandle.h"

#include "ProjectHistory.h"
#include "../../../../ProjectSettings.h"
#include "../../../../SelectionState.h"
#include "../../../../RefreshCode.h"
#include "Theme.h"
#include "../../../../../images/Cursors.h"
#include "../../../../HitTestResult.h"
#include "../../../../TrackPanel.h"
#include "../../../../TrackPanelAx.h"

#include "../WaveTrackUtils.h"

#include "WaveClipTrimHandle.h"



class SetWaveClipNameCommand : public AudacityCommand
{
public:
    static const ComponentInterfaceSymbol Symbol;

    ComponentInterfaceSymbol GetSymbol() const override
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

//Handle which is used to send mouse events to TextEditHelper
class WaveClipTitleEditHandle final : public UIHandle
{
    std::shared_ptr<TextEditHelper> mHelper;
public:

    WaveClipTitleEditHandle(const std::shared_ptr<TextEditHelper>& helper)
        : mHelper(helper)
    { }
   
   ~WaveClipTitleEditHandle()
   {
   }

    Result Click(const TrackPanelMouseEvent& event, AudacityProject* project) override
    {
        if (mHelper->OnClick(event.event, project))
            return RefreshCode::RefreshCell;
        return RefreshCode::RefreshNone;
    }

    Result Drag(const TrackPanelMouseEvent& event, AudacityProject* project) override
    {
        if (mHelper->OnDrag(event.event, project))
            return RefreshCode::RefreshCell;
        return RefreshCode::RefreshNone;
    }

    HitTestPreview Preview(const TrackPanelMouseState& state, AudacityProject* pProject) override
    {
        static auto ibeamCursor =
            ::MakeCursor(wxCURSOR_IBEAM, IBeamCursorXpm, 17, 16);
        return {
           XO("Click and drag to select text"),
           ibeamCursor.get()
        };
    }

    Result Release(const TrackPanelMouseEvent& event, AudacityProject* project, wxWindow*) override
    {
        if (mHelper->OnRelease(event.event, project))
            return RefreshCode::RefreshCell;
        return RefreshCode::RefreshNone;
    }

    Result Cancel(AudacityProject* project) override
    {
        if (mHelper)
        {
            mHelper->Cancel(project);
            mHelper.reset();
        }
        return RefreshCode::RefreshAll;
    }
};

WaveTrackAffordanceControls::WaveTrackAffordanceControls(const std::shared_ptr<Track>& pTrack)
    : CommonTrackCell(pTrack), mClipNameFont(wxFont(wxFontInfo()))
{
    if (auto trackList = pTrack->GetOwner())
    {
        mTrackListEventSubscription = trackList->Subscribe(
            *this, &WaveTrackAffordanceControls::OnTrackListEvent);
        if(auto project = trackList->GetOwner())
        {
            auto& viewInfo = ViewInfo::Get(*project);
            mSelectionChangeSubscription =
                viewInfo.selectedRegion.Subscribe(
                    *this,
                    &WaveTrackAffordanceControls::OnSelectionChange);
        }
    }
}

std::vector<UIHandlePtr> WaveTrackAffordanceControls::HitTest(const TrackPanelMouseState& state, const AudacityProject* pProject)
{
    std::vector<UIHandlePtr> results;

    auto px = state.state.m_x;
    auto py = state.state.m_y;

    const auto rect = state.rect;

    auto track = std::static_pointer_cast<WaveTrack>(FindTrack());

    {
        auto handle = WaveClipTrimHandle::HitAnywhere(
            mClipTrimHandle,
            track,
            pProject,
            state);

        if (handle)
            results.push_back(handle);
    }

    auto trackList = track->GetOwner();
    if ((std::abs(rect.GetTop() - py) <= WaveTrackView::kChannelSeparatorThickness / 2) 
        && trackList
        && !track->IsLeader())
    {
        //given that track is not a leader there always should be
        //another track before this one
        auto prev = std::prev(trackList->Find(track.get()));
        results.push_back(
            AssignUIHandlePtr(
                mResizeHandle, 
                std::make_shared<TrackPanelResizeHandle>((*prev)->shared_from_this(), py)
            )
        );
    }

    if (mTextEditHelper && mTextEditHelper->GetBBox().Contains(px, py))
    {
        results.push_back(
            AssignUIHandlePtr(
                mTitleEditHandle,
                std::make_shared<WaveClipTitleEditHandle>(mTextEditHelper)
            )
        );
    }

    auto editClipLock = mEditedClip.lock();
    const auto waveTrack = std::static_pointer_cast<WaveTrack>(track->SubstitutePendingChangedTrack());
    auto& zoomInfo = ViewInfo::Get(*pProject);
    for (const auto& clip : waveTrack->GetClips())
    {
        if (clip == editClipLock)
            continue;

        if (WaveTrackView::HitTest(*clip, zoomInfo, state.rect, {px, py}))
        {
            results.push_back(
                AssignUIHandlePtr(
                    mAffordanceHandle,
                    std::make_shared<WaveTrackAffordanceHandle>(track, clip)
                )
            );
            mFocusClip = clip;
            break;
        }
    }

    const auto& settings = ProjectSettings::Get(*pProject);
    const auto currentTool = settings.GetTool();
    if (currentTool == ToolCodes::multiTool || currentTool == ToolCodes::selectTool)
    {
        results.push_back(
            SelectHandle::HitTest(
                mSelectHandle, state, pProject,
                TrackView::Get(*track).shared_from_this()
            )
        );
    }

    return results;
}

void WaveTrackAffordanceControls::Draw(TrackPanelDrawingContext& context, const wxRect& rect, unsigned iPass)
{
    if (iPass == TrackArtist::PassBackground) {
        auto track = FindTrack();
        const auto artist = TrackArtist::Get(context);

        TrackArt::DrawBackgroundWithSelection(context, rect, track.get(), artist->blankSelectedBrush, artist->blankBrush);

        mLastVisibleClips.clear();
       
        const auto waveTrack = std::static_pointer_cast<WaveTrack>(track->SubstitutePendingChangedTrack());
        const auto& zoomInfo = *artist->pZoomInfo;
        {
            wxDCClipper dcClipper(context.dc, rect);

            context.dc.SetTextBackground(wxTransparentColor);
            context.dc.SetTextForeground(theTheme.Colour(clrClipNameText));
            context.dc.SetFont(mClipNameFont);

            auto px = context.lastState.m_x;
            auto py = context.lastState.m_y;

            for (const auto& clip : waveTrack->GetClips())
            {
                auto affordanceRect
                   = ClipParameters::GetClipRect(*clip.get(), zoomInfo, rect);

                if(!WaveTrackView::ClipDetailsVisible(*clip, zoomInfo, rect))
                {
                   TrackArt::DrawClipFolded(context.dc, affordanceRect);
                   continue;
                }

                const auto selected = GetSelectedClip().lock() == clip;
                const auto highlight = selected || affordanceRect.Contains(px, py);
                const auto titleRect = TrackArt::DrawClipAffordance(context.dc, affordanceRect, highlight, selected);
                if (mTextEditHelper && mEditedClip.lock() == clip)
                {
                    if(!mTextEditHelper->Draw(context.dc, titleRect))
                    {
                        mTextEditHelper->Cancel(nullptr);
                        TrackArt::DrawClipTitle(context.dc, titleRect, clip->GetName());
                    }
                }
                else if(TrackArt::DrawClipTitle(context.dc, titleRect, clip->GetName()))
                    mLastVisibleClips.push_back(clip.get());
            }
        }

    }
}

bool WaveTrackAffordanceControls::IsClipNameVisible(const WaveClip &clip) const noexcept
{
    return std::find(mLastVisibleClips.begin(),
                     mLastVisibleClips.end(),
                     &clip) != mLastVisibleClips.end();
}

bool WaveTrackAffordanceControls::StartEditClipName(AudacityProject& project, const std::shared_ptr<WaveClip>& clip)
{
    bool useDialog{ false };
    gPrefs->Read(wxT("/GUI/DialogForNameNewLabel"), &useDialog, false);

    if (useDialog)
    {
        SetWaveClipNameCommand Command;
        auto oldName = clip->GetName();
        Command.mName = oldName;
        auto result = Command.PromptUser(&GetProjectFrame(project));
        if (result && Command.mName != oldName)
        {
            clip->SetName(Command.mName);
            ProjectHistory::Get(project).PushState(XO("Modified Clip Name"),
                XO("Clip Name Edit"));
        }
    }
    else if(clip != mEditedClip.lock())
    {
        if(!IsClipNameVisible(*clip))
           return false;
       
        if (mTextEditHelper)
            mTextEditHelper->Finish(&project);

        mEditedClip = clip;
        mTextEditHelper = MakeTextEditHelper(clip->GetName());
    }
   
    return true;
}

std::weak_ptr<WaveClip> WaveTrackAffordanceControls::GetSelectedClip() const
{
    if (auto handle = mAffordanceHandle.lock())
    {
        return handle->Clicked() ? mFocusClip : std::weak_ptr<WaveClip>();
    }
    return {};
}

namespace {

auto FindAffordance(WaveTrack &track)
{
   auto &view = TrackView::Get( track );
   auto pAffordance = view.GetAffordanceControls();
   return std::dynamic_pointer_cast<WaveTrackAffordanceControls>(
      pAffordance );
}

std::pair<WaveTrack *, std::shared_ptr<WaveClip>>
SelectedClipOfFocusedTrack(AudacityProject &project)
{
   // Note that TrackFocus may change its state as a side effect, defining
   // a track focus if there was none
   if (auto pWaveTrack =
      dynamic_cast<WaveTrack *>(TrackFocus::Get(project).Get())) {
      for (auto pChannel : TrackList::Channels(pWaveTrack)) {
         if (FindAffordance(*pChannel)) {
            auto &viewInfo = ViewInfo::Get(project);
            auto &clips = pChannel->GetClips();
            auto begin = clips.begin(), end = clips.end(),
               iter = WaveTrackUtils::SelectedClip(viewInfo, begin, end);
            if (iter != end)
               return { pChannel, *iter };
         }
      }
   }
   return { nullptr, nullptr };
}

// condition for enabling the command
const ReservedCommandFlag &SomeClipIsSelectedFlag()
{
   static ReservedCommandFlag flag{
      [](const AudacityProject &project){
         return nullptr !=
            // const_cast isn't pretty but not harmful in this case
            SelectedClipOfFocusedTrack(const_cast<AudacityProject&>(project))
               .second;
      }
   };
   return flag;
}

}

unsigned WaveTrackAffordanceControls::CaptureKey(wxKeyEvent& event, ViewInfo& viewInfo, wxWindow* pParent, AudacityProject* project)
{
    if (!mTextEditHelper 
       || !mTextEditHelper->CaptureKey(event.GetKeyCode(), event.GetModifiers()))
       // Handle the event if it can be processed by the text editor (if any)
       event.Skip();
    return RefreshCode::RefreshNone;
}


unsigned WaveTrackAffordanceControls::KeyDown(wxKeyEvent& event, ViewInfo& viewInfo, wxWindow*, AudacityProject* project)
{
    auto keyCode = event.GetKeyCode();
    
    if (mTextEditHelper)
    {
       if (!mTextEditHelper->OnKeyDown(keyCode, event.GetModifiers(), project) 
          && !TextEditHelper::IsGoodEditKeyCode(keyCode))
          event.Skip();

       return RefreshCode::RefreshCell;
    }
    return RefreshCode::RefreshNone;
}

unsigned WaveTrackAffordanceControls::Char(wxKeyEvent& event, ViewInfo& viewInfo, wxWindow* pParent, AudacityProject* project)
{
    if (mTextEditHelper && mTextEditHelper->OnChar(event.GetUnicodeKey(), project))
        return RefreshCode::RefreshCell;
    return RefreshCode::RefreshNone;
}

unsigned WaveTrackAffordanceControls::LoseFocus(AudacityProject *)
{
   return ExitTextEditing();
}

void WaveTrackAffordanceControls::OnTextEditFinished(AudacityProject* project, const wxString& text)
{
    if (auto lock = mEditedClip.lock())
    {
        if (text != lock->GetName()) {
            lock->SetName(text);

            ProjectHistory::Get(*project).PushState(XO("Modified Clip Name"),
                XO("Clip Name Edit"));
        }
    }
    ResetClipNameEdit();
}

void WaveTrackAffordanceControls::OnTextEditCancelled(AudacityProject* project)
{
    ResetClipNameEdit();
}

void WaveTrackAffordanceControls::OnTextModified(AudacityProject* project, const wxString& text)
{
    //Nothing to do
}

void WaveTrackAffordanceControls::OnTextContextMenu(AudacityProject* project, const wxPoint& position)
{
}

void WaveTrackAffordanceControls::ResetClipNameEdit()
{
    mTextEditHelper.reset();
    mEditedClip.reset();
}

void WaveTrackAffordanceControls::OnTrackListEvent(const TrackListEvent& evt)
{
    if (evt.mType == TrackListEvent::SELECTION_CHANGE)
       ExitTextEditing();
}

void WaveTrackAffordanceControls::OnSelectionChange(NotifyingSelectedRegionMessage)
{
    ExitTextEditing();
}

unsigned WaveTrackAffordanceControls::ExitTextEditing()
{
    using namespace RefreshCode;
    if (mTextEditHelper)
    {
        if (auto trackList = FindTrack()->GetOwner())
        {
            mTextEditHelper->Finish(trackList->GetOwner());
        }
        ResetClipNameEdit();
        return RefreshCell;
    }
    return RefreshNone;
}

bool WaveTrackAffordanceControls::OnTextCopy(AudacityProject& project)
{
   if (mTextEditHelper)
   {
      mTextEditHelper->CopySelectedText(project);
      return true;
   }
   return false;
}

bool WaveTrackAffordanceControls::OnTextCut(AudacityProject& project)
{
   if (mTextEditHelper)
   {
      mTextEditHelper->CutSelectedText(project);
      return true;
   }
   return false;
}

bool WaveTrackAffordanceControls::OnTextPaste(AudacityProject& project)
{
   if (mTextEditHelper)
   {
      mTextEditHelper->PasteSelectedText(project);
      return true;
   }
   return false;
}

bool WaveTrackAffordanceControls::OnTextSelect(AudacityProject& project)
{
   if (mTextEditHelper)
   {
      mTextEditHelper->SelectAll();
      return true;
   }
   return false;
}

unsigned WaveTrackAffordanceControls::OnAffordanceClick(const TrackPanelMouseEvent& event, AudacityProject* project)
{
    auto& viewInfo = ViewInfo::Get(*project);
    if (mTextEditHelper)
    {
        if (auto lock = mEditedClip.lock())
        {
            auto affordanceRect = ClipParameters::GetClipRect(*lock.get(), viewInfo, event.rect);
            if (!affordanceRect.Contains(event.event.GetPosition()))
               return ExitTextEditing();
        }
    }
    else if (auto lock = mFocusClip.lock())
    {
        if (event.event.LeftDClick())
        {
            auto affordanceRect = ClipParameters::GetClipRect(*lock.get(), viewInfo, event.rect);
            if (affordanceRect.Contains(event.event.GetPosition()) &&
                StartEditClipName(*project, lock))
            {
                event.event.Skip(false);
                return RefreshCode::RefreshCell;
            }
        }
    }
    return RefreshCode::RefreshNone;
}

void WaveTrackAffordanceControls::StartEditSelectedClipName(AudacityProject& project)
{
   const auto [track, clip] = SelectedClipOfFocusedTrack(project);
   if(track == nullptr || track != FindTrack().get() || clip == nullptr)
      return;
   StartEditClipName(project, clip);
}

std::shared_ptr<TextEditHelper> WaveTrackAffordanceControls::MakeTextEditHelper(const wxString& text)
{
    auto helper = std::make_shared<TextEditHelper>(shared_from_this(), text, mClipNameFont);
    helper->SetTextColor(theTheme.Colour(clrClipNameText));
    helper->SetTextSelectionColor(theTheme.Colour(clrClipNameTextSelection));
    return helper; 
}

// Register a menu item

namespace {

// Menu handler functions

void OnEditClipName(const CommandContext &context)
{
   auto &project = context.project;
   
   if(auto pWaveTrack = dynamic_cast<WaveTrack *>(TrackFocus::Get(project).Get()))
   {
      if(auto pAffordance = FindAffordance(*pWaveTrack))
      {
         pAffordance->StartEditSelectedClipName(project);
         // Refresh so the cursor appears
         TrackPanel::Get(project).RefreshTrack(pWaveTrack);
      }
   }
}

using namespace MenuTable;

// Register menu items

AttachedItem sAttachment{ wxT("Edit/Other"),
   Command( L"RenameClip", XXO("Rename Clip..."),
      OnEditClipName, SomeClipIsSelectedFlag(), wxT("Ctrl+F2") )
};

}
