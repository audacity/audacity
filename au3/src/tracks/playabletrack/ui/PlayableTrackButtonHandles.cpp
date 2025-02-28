/**********************************************************************

Audacity: A Digital Audio Editor

PlayableTrackButtonHandles.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/
#include "PlayableTrackButtonHandles.h"
#include "PlayableTrack.h"
#include "PlayableTrackControls.h"
#include "CommandManager.h"
#include "Project.h"
#include "../../../RefreshCode.h"
#include "../../../RealtimeEffectPanel.h"
#include "SampleTrack.h"
#include "TrackFocus.h"
#include "../../ui/CommonTrackInfo.h"
#include "../../../TrackPanelMouseEvent.h"
#include "../../../TrackUtilities.h"

#include <wx/window.h>

MuteButtonHandle::MuteButtonHandle
    (const std::shared_ptr<Track>& pTrack, const wxRect& rect)
    : ButtonHandle{pTrack, rect}
{}

MuteButtonHandle::~MuteButtonHandle()
{
}

UIHandle::Result MuteButtonHandle::CommitChanges
    (const wxMouseEvent& event, AudacityProject* pProject, wxWindow*)
{
    auto pTrack = mpTrack.lock();
    if (dynamic_cast<PlayableTrack*>(pTrack.get())) {
        TrackUtilities::DoTrackMute(*pProject, *pTrack, event.ShiftDown());
    }

    return RefreshCode::RefreshNone;
}

TranslatableString MuteButtonHandle::Tip(
    const wxMouseState&, AudacityProject& project) const
{
    auto name = XO("Mute");
    auto focused
        =TrackFocus::Get(project).Get() == GetTrack().get();
    if (!focused) {
        return name;
    }

    auto& commandManager = CommandManager::Get(project);
    ComponentInterfaceSymbol command{ wxT("TrackMute"), name };
    return commandManager.DescribeCommandsAndShortcuts(&command, 1u);
}

UIHandlePtr MuteButtonHandle::HitTest
    (std::weak_ptr<MuteButtonHandle>& holder,
    const wxMouseState& state, const wxRect& rect,
    const AudacityProject* pProject, const std::shared_ptr<Track>& pTrack)
{
    wxRect buttonRect;
    if (pTrack) {
        PlayableTrackControls::GetMuteSoloRect(rect, buttonRect, false,
                                               pTrack.get());
    }
    if (CommonTrackInfo::HideTopItem(rect, buttonRect)) {
        return {}
    }

    if (pTrack && buttonRect.Contains(state.m_x, state.m_y)) {
        auto result = std::make_shared<MuteButtonHandle>(pTrack, buttonRect);
        result = AssignUIHandlePtr(holder, result);
        return result;
    } else {
        return {}
    }
}

////////////////////////////////////////////////////////////////////////////////

SoloButtonHandle::SoloButtonHandle
    (const std::shared_ptr<Track>& pTrack, const wxRect& rect)
    : ButtonHandle{pTrack, rect}
{}

SoloButtonHandle::~SoloButtonHandle()
{
}

UIHandle::Result SoloButtonHandle::CommitChanges
    (const wxMouseEvent& event, AudacityProject* pProject, wxWindow* WXUNUSED(pParent))
{
    auto pTrack = mpTrack.lock();
    if (dynamic_cast<PlayableTrack*>(pTrack.get())) {
        TrackUtilities::DoTrackSolo(*pProject, *pTrack, event.ShiftDown());
    }

    return RefreshCode::RefreshNone;
}

TranslatableString SoloButtonHandle::Tip(
    const wxMouseState&, AudacityProject& project) const
{
    auto name = XO("Solo");
    auto focused
        =TrackFocus::Get(project).Get() == GetTrack().get();
    if (!focused) {
        return name;
    }

    auto& commandManager = CommandManager::Get(project);
    ComponentInterfaceSymbol command{ wxT("TrackSolo"), name };
    return commandManager.DescribeCommandsAndShortcuts(&command, 1u);
}

UIHandlePtr SoloButtonHandle::HitTest
    (std::weak_ptr<SoloButtonHandle>& holder,
    const wxMouseState& state, const wxRect& rect,
    const AudacityProject* pProject, const std::shared_ptr<Track>& pTrack)
{
    wxRect buttonRect;
    if (pTrack) {
        PlayableTrackControls::GetMuteSoloRect(rect, buttonRect, true,
                                               pTrack.get());
    }

    if (CommonTrackInfo::HideTopItem(rect, buttonRect)) {
        return {}
    }

    if (pTrack && buttonRect.Contains(state.m_x, state.m_y)) {
        auto result = std::make_shared<SoloButtonHandle>(pTrack, buttonRect);
        result = AssignUIHandlePtr(holder, result);
        return result;
    } else {
        return {}
    }
}

////////////////////////////////////////////////////////////////////////////////

EffectsButtonHandle::EffectsButtonHandle
    (const std::shared_ptr<Track>& pTrack, const wxRect& rect)
    : ButtonHandle{pTrack, rect}
{}

EffectsButtonHandle::~EffectsButtonHandle()
{
}

UIHandle::Result EffectsButtonHandle::CommitChanges
    (const wxMouseEvent& event, AudacityProject* pProject, wxWindow* pParent)
{
    RealtimeEffectPanel::Get(*pProject).ShowPanel(
        dynamic_cast<SampleTrack*>(mpTrack.lock().get()), true);
    return RefreshCode::RefreshNone;
}

TranslatableString EffectsButtonHandle::Tip(
    const wxMouseState&, AudacityProject& project) const
{
    auto name = XO("Effects");
    auto focused
        =TrackFocus::Get(project).Get() == GetTrack().get();
    if (!focused) {
        return name;
    } else {
        return name;
        // Instead supply shortcut when "TrackEffects" is defined
        /*
        auto &commandManager = CommandManager::Get( project );
        ComponentInterfaceSymbol command{ wxT("TrackEffects"), name };
        return commandManager.DescribeCommandsAndShortcuts( &command, 1u );
         */
    }
}

UIHandlePtr EffectsButtonHandle::HitTest
    (std::weak_ptr<EffectsButtonHandle>& holder,
    const wxMouseState& state, const wxRect& rect,
    const AudacityProject* pProject, const std::shared_ptr<Track>& pTrack)
{
    wxRect buttonRect;
    if (pTrack) {
        PlayableTrackControls::GetEffectsButtonRect(rect, buttonRect,
                                                    pTrack.get());
    }

    if (CommonTrackInfo::HideTopItem(rect, buttonRect)) {
        return {}
    }

    if (pTrack && buttonRect.Contains(state.m_x, state.m_y)) {
        auto result = std::make_shared<EffectsButtonHandle>(pTrack, buttonRect);
        result = AssignUIHandlePtr(holder, result);
        return result;
    } else {
        return {}
    }
}
