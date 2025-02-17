/**********************************************************************

Audacity: A Digital Audio Editor

TrackButtonHandles.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "TrackButtonHandles.h"

#include "PendingTracks.h"
#include "Project.h"
#include "ProjectAudioIO.h"
#include "../../ProjectAudioManager.h"
#include "ProjectHistory.h"
#include "../../SelectUtilities.h"
#include "../../RefreshCode.h"
#include "Track.h"
#include "TrackFocus.h"
#include "CommonTrackInfo.h"
#include "../../TrackPanel.h"
#include "../../TrackUtilities.h"
#include "CommandManager.h"
#include "../../tracks/ui/ChannelView.h"

MinimizeButtonHandle::MinimizeButtonHandle
    (const std::shared_ptr<Track>& pTrack, const wxRect& rect)
    : ButtonHandle{pTrack, rect}
{}

MinimizeButtonHandle::~MinimizeButtonHandle()
{
}

UIHandle::Result MinimizeButtonHandle::CommitChanges
    (const wxMouseEvent&, AudacityProject* pProject, wxWindow*)
{
    using namespace RefreshCode;

    if (auto pTrack = mpTrack.lock()) {
        auto channels = pTrack->Channels();
        bool wasMinimized = ChannelView::Get(**channels.begin()).GetMinimized();
        for (auto pChannel : channels) {
            ChannelView::Get(*pChannel).SetMinimized(!wasMinimized);
        }
        ProjectHistory::Get(*pProject).ModifyState(true);

        // Redraw all tracks when any one of them expands or contracts
        // (Could we invent a return code that draws only those at or below
        // the affected track?)
        return RefreshAll | FixScrollbars;
    }

    return RefreshNone;
}

TranslatableString MinimizeButtonHandle::Tip(
    const wxMouseState&, AudacityProject&) const
{
    auto pTrack = GetTrack();
    return ChannelView::Get(*pTrack->GetChannel(0)).GetMinimized()
           ? XO("Expand") : XO("Collapse");
}

UIHandlePtr MinimizeButtonHandle::HitTest
    (std::weak_ptr<MinimizeButtonHandle>& holder,
    const wxMouseState& state, const wxRect& rect, TrackPanelCell* pCell)
{
    wxRect buttonRect;
    CommonTrackInfo::GetMinimizeRect(rect, buttonRect);

    if (buttonRect.Contains(state.m_x, state.m_y)) {
        auto pTrack = static_cast<CommonTrackPanelCell*>(pCell)->FindTrack();
        auto result = std::make_shared<MinimizeButtonHandle>(pTrack, buttonRect);
        result = AssignUIHandlePtr(holder, result);
        return result;
    } else {
        return {}
    }
}

////////////////////////////////////////////////////////////////////////////////

CloseButtonHandle::CloseButtonHandle
    (const std::shared_ptr<Track>& pTrack, const wxRect& rect)
    : ButtonHandle{pTrack, rect}
{}

CloseButtonHandle::~CloseButtonHandle()
{
}

UIHandle::Result CloseButtonHandle::CommitChanges(const wxMouseEvent&,
                                                  AudacityProject* pProject, wxWindow*)
{
    using namespace RefreshCode;
    Result result = RefreshNone;

    auto pTrack = mpTrack.lock();
    if (pTrack) {
        auto& toRemove = PendingTracks::Get(*pProject)
                         .SubstitutePendingChangedTrack(*pTrack);
        ProjectAudioManager::Get(*pProject).StopIfPaused();
        if (!ProjectAudioIO::Get(*pProject).IsAudioActive()) {
            // This pushes an undo item:
            TrackUtilities::DoRemoveTrack(*pProject, toRemove);
            // Redraw all tracks when any one of them closes
            // (Could we invent a return code that draws only those at or below
            // the affected track?)
            result |= Resize | RefreshAll | FixScrollbars | DestroyedCell;
        }
    }

    return result;
}

TranslatableString CloseButtonHandle::Tip(
    const wxMouseState&, AudacityProject& project) const
{
    auto name = XO("Delete Track");
    auto focused
        =TrackFocus::Get(project).Get() == GetTrack().get();
    if (!focused) {
        return name;
    }

    auto& commandManager = CommandManager::Get(project);
    ComponentInterfaceSymbol command{ wxT("TrackClose"), name };
    return commandManager.DescribeCommandsAndShortcuts(&command, 1u);
}

UIHandlePtr CloseButtonHandle::HitTest
    (std::weak_ptr<CloseButtonHandle>& holder,
    const wxMouseState& state, const wxRect& rect, TrackPanelCell* pCell)
{
    wxRect buttonRect;
    CommonTrackInfo::GetCloseBoxRect(rect, buttonRect);

    if (buttonRect.Contains(state.m_x, state.m_y)) {
        auto pTrack = static_cast<CommonTrackPanelCell*>(pCell)->FindTrack();
        auto result = std::make_shared<CloseButtonHandle>(pTrack, buttonRect);
        result = AssignUIHandlePtr(holder, result);
        return result;
    } else {
        return {}
    }
}

////////////////////////////////////////////////////////////////////////////////

MenuButtonHandle::MenuButtonHandle
    (const std::shared_ptr<TrackPanelCell>& pCell,
    const std::shared_ptr<Track>& pTrack, const wxRect& rect)
    : ButtonHandle{pTrack, rect}
    , mpCell{pCell}
{}

MenuButtonHandle::~MenuButtonHandle()
{
}

UIHandle::Result MenuButtonHandle::CommitChanges
    (const wxMouseEvent&, AudacityProject* pProject, wxWindow* WXUNUSED(pParent))
{
    auto& trackPanel = TrackPanel::Get(*pProject);
    auto pCell = mpCell.lock();
    if (!pCell) {
        return RefreshCode::Cancelled;
    }
    auto pTrack
        =static_cast<CommonTrackPanelCell*>(pCell.get())->FindTrack();
    if (!pTrack) {
        return RefreshCode::Cancelled;
    }
    trackPanel.CallAfter(
        [&trackPanel, pTrack]{ trackPanel.OnTrackMenu(pTrack.get()); });
    return RefreshCode::RefreshNone;
}

TranslatableString MenuButtonHandle::Tip(
    const wxMouseState&, AudacityProject& project) const
{
    auto name = XO("Open menu...");
    auto focused
        =TrackFocus::Get(project).Get() == GetTrack().get();
    if (!focused) {
        return name;
    }

    auto& commandManager = CommandManager::Get(project);
    ComponentInterfaceSymbol command{ wxT("TrackMenu"), name };
    return commandManager.DescribeCommandsAndShortcuts(&command, 1u);
}

UIHandlePtr MenuButtonHandle::HitTest
    (std::weak_ptr<MenuButtonHandle>& holder,
    const wxMouseState& state, const wxRect& rect,
    const std::shared_ptr<TrackPanelCell>& pCell)
{
    wxRect buttonRect;
    CommonTrackInfo::GetTrackMenuButtonRect(rect, buttonRect);

    if (buttonRect.Contains(state.m_x, state.m_y)) {
        auto pTrack = static_cast<CommonTrackPanelCell*>(pCell.get())->FindTrack();
        auto result = std::make_shared<MenuButtonHandle>(pCell, pTrack, buttonRect);
        result = AssignUIHandlePtr(holder, result);
        return result;
    } else {
        return {}
    }
}
