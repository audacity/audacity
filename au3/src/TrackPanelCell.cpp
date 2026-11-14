/**********************************************************************

 Audacity: A Digital Audio Editor

 TrackPanelCell.cpp

 Paul Licameli split from TrackPanel.cpp

 **********************************************************************/

#include "TrackPanelCell.h"

#include <wx/event.h>
#include "HitTestResult.h"
#include "RefreshCode.h"

TrackPanelNode::TrackPanelNode()
{
}

TrackPanelNode::~TrackPanelNode()
{
}

TrackPanelGroup::TrackPanelGroup()
{
}

TrackPanelGroup::~TrackPanelGroup()
{
}

TrackPanelCell::~TrackPanelCell()
{
}

HitTestPreview TrackPanelCell::DefaultPreview
    (const TrackPanelMouseState&, const AudacityProject*)
{
    return {};
}

unsigned TrackPanelCell::HandleWheelRotation
    (const TrackPanelMouseEvent&, AudacityProject*)
{
    return RefreshCode::Cancelled;
}

unsigned TrackPanelCell::DoContextMenu
    (const wxRect&, wxWindow*, const wxPoint*, AudacityProject*)
{
    return RefreshCode::RefreshNone;
}

unsigned TrackPanelCell::CaptureKey(
    wxKeyEvent& event, ViewInfo&, wxWindow*, AudacityProject*)
{
    event.Skip();
    return RefreshCode::RefreshNone;
}

unsigned TrackPanelCell::KeyDown(
    wxKeyEvent& event, ViewInfo&, wxWindow*, AudacityProject*)
{
    event.Skip();
    return RefreshCode::RefreshNone;
}

unsigned TrackPanelCell::KeyUp(
    wxKeyEvent& event, ViewInfo&, wxWindow*, AudacityProject*)
{
    event.Skip();
    return RefreshCode::RefreshNone;
}

unsigned TrackPanelCell::Char(
    wxKeyEvent& event, ViewInfo&, wxWindow*, AudacityProject*)
{
    event.Skip();
    return RefreshCode::RefreshNone;
}

unsigned TrackPanelCell::LoseFocus(AudacityProject*)
{
    return RefreshCode::RefreshNone;
}
