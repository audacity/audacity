/**********************************************************************

Audacity: A Digital Audio Editor

LabelTextHandle.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "LabelTextHandle.h"

#include "LabelTrackView.h"

#include "../../../HitTestResult.h"
#include "LabelTrack.h"
#include "ProjectAudioIO.h"
#include "ProjectHistory.h"
#include "../../../RefreshCode.h"
#include "SelectionState.h"
#include "../../../TrackPanelMouseEvent.h"
#include "ViewInfo.h"
#include "../../../../images/Cursors.h"

#include <wx/clipbrd.h>

LabelTextHandle::LabelTextHandle
    (const std::shared_ptr<LabelTrack>& pLT, int labelNum)
    : mpLT{pLT}
    , mLabelNum{labelNum}
{
}

void LabelTextHandle::Enter(bool, AudacityProject*)
{
}

HitTestPreview LabelTextHandle::HitPreview()
{
    static auto ibeamCursor
        =::MakeCursor(wxCURSOR_IBEAM, IBeamCursorXpm, 17, 16);
    return {
        XO("Click to edit label text"),
        ibeamCursor.get()
    };
}

UIHandlePtr LabelTextHandle::HitTest
    (std::weak_ptr<LabelTextHandle>& holder,
    const wxMouseState& state, const std::shared_ptr<LabelTrack>& pLT)
{
    // If Control is down, let the select handle be hit instead
    int labelNum;
    if (!state.ControlDown()
        && (labelNum
                =LabelTrackView::OverATextBox(*pLT, state.m_x, state.m_y)) >= 0) {
        auto result = std::make_shared<LabelTextHandle>(pLT, labelNum);
        result = AssignUIHandlePtr(holder, result);
        return result;
    }

    return {};
}

LabelTextHandle::~LabelTextHandle()
{
}

std::shared_ptr<const Track> LabelTextHandle::FindTrack() const
{
    return mpLT.lock();
}

void LabelTextHandle::HandleTextClick(AudacityProject& project, const wxMouseEvent& evt)
{
    auto pTrack = mpLT.lock();
    if (!pTrack) {
        return;
    }

    auto& view = LabelTrackView::Get(*pTrack);
    if (evt.ButtonDown()) {
        const auto selIndex = LabelTrackView::OverATextBox(*pTrack, evt.m_x, evt.m_y);
        if (selIndex != -1) {
            if (evt.LeftDown()) {
                mRightDragging = false;
                // Find the NEW drag end
                auto position = view.FindCursorPosition(selIndex, evt.m_x);

                // Anchor shift-drag at the farther end of the previous highlight
                // that is farther from the click, on Mac, for consistency with
                // its text editors, but on the others, re-use the previous
                // anchor.
                auto initial = view.GetInitialCursorPosition();
                if (evt.ShiftDown()) {
#ifdef __WXMAC__
                    // Set the drag anchor at the end of the previous selection
                    // that is farther from the NEW drag end
                    const auto current = view.GetCurrentCursorPosition();
                    if (abs(position - current) > abs(position - initial)) {
                        initial = current;
                    }
#else
                    // initial position remains as before
#endif
                } else {
                    initial = position;
                }

                view.SetTextSelection(selIndex, initial, position);
            } else {
                if (!view.IsTextSelected(project)) {
                    auto position = view.FindCursorPosition(selIndex, evt.m_x);
                    view.SetTextSelection(selIndex, position, position);
                }
                // Actually this might be right or middle down
                mRightDragging = true;
            }
            // Middle click on GTK: paste from primary selection
#if defined(__WXGTK__) && (HAVE_GTK)
            if (evt.MiddleDown()) {
                // Check for a click outside of the selected label's text box; in this
                // case PasteSelectedText() will start a NEW label at the click
                // location
                if (!LabelTrackView::OverTextBox(&labelStruct, evt.m_x, evt.m_y)) {
                    view.ResetTextSelection();
                }
                double t = zoomInfo.PositionToTime(evt.m_x, r.x);
                newSel = SelectedRegion(t, t);
            }
#endif
        }
#if defined(__WXGTK__) && (HAVE_GTK)
        if (evt.MiddleDown()) {
            // Paste text, making a NEW label if none is selected.
            wxTheClipboard->UsePrimarySelection(true);
            view.PasteSelectedText(project, newSel.t0(), newSel.t1());
            wxTheClipboard->UsePrimarySelection(false);
        }
#endif
    }
}

bool LabelTextHandle::HandlesRightClick()
{
    return true;
}

UIHandle::Result LabelTextHandle::Click
    (const TrackPanelMouseEvent& evt, AudacityProject* pProject)
{
    auto pLT = mpLT.lock();
    if (!pLT) {
        return RefreshCode::Cancelled;
    }

    auto result = LabelDefaultClickHandle::Click(evt, pProject);

    const wxMouseEvent& event = evt.event;
    auto& viewInfo = ViewInfo::Get(*pProject);

    HandleTextClick(*pProject, event);

    return result | RefreshCode::RefreshCell;
}

void LabelTextHandle::HandleTextDragRelease(
    AudacityProject& project, const wxMouseEvent& evt)
{
    auto pTrack = mpLT.lock();
    if (!pTrack) {
        return;
    }
    auto& view = LabelTrackView::Get(*pTrack);

    if (evt.LeftUp()) {
#if 0
        // AWD: Due to wxWidgets bug #7491 (fix not ported to 2.8 branch) we
        // should never write the primary selection. We can enable this block
        // when we move to the 3.0 branch (or if a fixed 2.8 version is released
        // and we can do a runtime version check)
#if defined (__WXGTK__) && defined (HAVE_GTK)
        // On GTK, if we just dragged out a text selection, set the primary
        // selection
        if (mInitialCursorPos != mCurrentCursorPos) {
            wxTheClipboard->UsePrimarySelection(true);
            CopySelectedText();
            wxTheClipboard->UsePrimarySelection(false);
        }
#endif
#endif

        return;
    }

    if (evt.Dragging()) {
        auto index = view.GetTextEditIndex(project);
        if (!mRightDragging && index != -1) {
            // Update drag end
            view.SetCurrentCursorPosition(view.FindCursorPosition(index, evt.m_x));
        }
        return;
    }

    if (evt.RightUp()) {
        auto index = view.GetTextEditIndex(project);
        if (index != -1
            && LabelTrackView::OverTextBox(pTrack->GetLabel(index), evt.m_x, evt.m_y)) {
            // popup menu for editing
            // TODO: handle context menus via CellularPanel?
            view.ShowContextMenu(project);
        }
    }

    return;
}

UIHandle::Result LabelTextHandle::Drag
    (const TrackPanelMouseEvent& evt, AudacityProject* pProject)
{
    auto& project = *pProject;
    using namespace RefreshCode;
    auto result = LabelDefaultClickHandle::Drag(evt, pProject);

    const wxMouseEvent& event = evt.event;
    auto pLT = TrackList::Get(*pProject).Lock(mpLT);
    if (pLT) {
        HandleTextDragRelease(project, event);
    }

    // locate the initial mouse position
    if (event.LeftIsDown()) {
        if (mLabelTrackStartXPos == -1) {
            mLabelTrackStartXPos = event.m_x;
            mLabelTrackStartYPos = event.m_y;

            auto pView = pLT ? &LabelTrackView::Get(*pLT) : nullptr;
            if (pLT && (pView->GetTextEditIndex(project) != -1)
                && LabelTrackView::OverTextBox(
                    pLT->GetLabel(pView->GetTextEditIndex(project)),
                    mLabelTrackStartXPos,
                    mLabelTrackStartYPos)) {
                mLabelTrackStartYPos = -1;
            }
        }
        // if initial mouse position in the text box
        // then only drag text
        if (mLabelTrackStartYPos == -1) {
            result |= RefreshCell;
        }
    }

    return result;
}

HitTestPreview LabelTextHandle::Preview
    (const TrackPanelMouseState&, AudacityProject*)
{
    return HitPreview();
}

UIHandle::Result LabelTextHandle::Release
    (const TrackPanelMouseEvent& evt, AudacityProject* pProject,
    wxWindow* pParent)
{
    auto result = LabelDefaultClickHandle::Release(evt, pProject, pParent);

    // Only selected a part of a text string and changed track selectedness.
    // No undoable effects.

    const wxMouseEvent& event = evt.event;
    auto pLT = TrackList::Get(*pProject).Lock(mpLT);
    if (pLT) {
        HandleTextDragRelease(*pProject, event);
    }

    // handle mouse left button up
    if (event.LeftUp()) {
        mLabelTrackStartXPos = -1;
    }

    return result | RefreshCode::RefreshNone;
}

UIHandle::Result LabelTextHandle::Cancel(AudacityProject* pProject)
{
    auto result = LabelDefaultClickHandle::Cancel(pProject);
    return result | RefreshCode::RefreshAll;
}
