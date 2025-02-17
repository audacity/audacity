/*!********************************************************************

   Audacity: A Digital Audio Editor

   @file MovableControl.cpp

   @author Vitaly Sverchinsky

**********************************************************************/

#include "MovableControl.h"
#include <wx/sizer.h>
#include <wx/wupdlock.h>

wxDEFINE_EVENT(EVT_MOVABLE_CONTROL_DRAG_STARTED, MovableControlEvent);
wxDEFINE_EVENT(EVT_MOVABLE_CONTROL_DRAG_POSITION, MovableControlEvent);
wxDEFINE_EVENT(EVT_MOVABLE_CONTROL_DRAG_FINISHED, MovableControlEvent);

MovableControlEvent::MovableControlEvent(wxEventType eventType, int winid)
    : wxCommandEvent(eventType, winid)
{
}

void MovableControlEvent::SetSourceIndex(int index) noexcept
{
    mSourceIndex = index;
}

int MovableControlEvent::GetSourceIndex() const noexcept
{
    return mSourceIndex;
}

void MovableControlEvent::SetTargetIndex(int index) noexcept
{
    mTargetIndex = index;
}

int MovableControlEvent::GetTargetIndex() const noexcept
{
    return mTargetIndex;
}

wxEvent* MovableControlEvent::Clone() const
{
    return new MovableControlEvent(*this);
}

MovableControl::MovableControl(wxWindow* parent,
                               wxWindowID id,
                               const wxPoint& pos,
                               const wxSize& size,
                               long style,
                               const wxString& name)
{
    Create(parent, id, pos, size, style, name);
}

void MovableControl::Create(wxWindow* parent,
                            wxWindowID id,
                            const wxPoint& pos,
                            const wxSize& size,
                            long style,
                            const wxString& name)
{
    wxWindow::Create(parent, id, pos, size, style, name);
    Bind(wxEVT_LEFT_DOWN, &MovableControl::OnMouseDown, this);
    Bind(wxEVT_LEFT_UP, &MovableControl::OnMouseUp, this);
    Bind(wxEVT_MOTION, &MovableControl::OnMove, this);
    Bind(wxEVT_KEY_DOWN, &MovableControl::OnKeyDown, this);
    Bind(wxEVT_MOUSE_CAPTURE_LOST, &MovableControl::OnMouseCaptureLost, this);
}

void MovableControl::ProcessDragEvent(wxWindow* target, wxEventType eventType)
{
    MovableControlEvent event(eventType);
    event.SetSourceIndex(mSourceIndex);
    event.SetTargetIndex(mTargetIndex);
    event.SetEventObject(this);
    target->GetEventHandler()->ProcessEvent(event);
}

int MovableControl::FindIndexInParent() const
{
    auto parent = GetParent();
    if (!parent) {
        return -1;
    }

    if (auto sizer = parent->GetSizer()) {
        for (size_t i = 0, count = sizer->GetItemCount(); i < count; ++i) {
            if (sizer->GetItem(i)->GetWindow() == this) {
                return static_cast<int>(i);
            }
        }
    }
    return -1;
}

void MovableControl::OnKeyDown(wxKeyEvent& evt)
{
    const auto keyCode = evt.GetKeyCode();
    if (evt.AltDown() && (keyCode == WXK_DOWN || keyCode == WXK_UP)) {
#ifdef __WXOSX__
        {//don't allow auto-repeats
            static long lastEventTimestamp = 0;
            if (lastEventTimestamp == evt.GetTimestamp()) {
                return;//don't skip
            }
            lastEventTimestamp = evt.GetTimestamp();
        }
#endif
        const auto sourceIndex = FindIndexInParent();
        if (sourceIndex == -1) {
            evt.Skip();
            return;
        }

        const auto targetIndex = std::clamp(
            keyCode == WXK_DOWN ? sourceIndex + 1 : sourceIndex - 1,
            0,
            static_cast<int>(GetParent()->GetSizer()->GetItemCount()) - 1
            );
        if (sourceIndex != targetIndex) {
            mSourceIndex = sourceIndex;
            mTargetIndex = targetIndex;
            ProcessDragEvent(GetParent(), EVT_MOVABLE_CONTROL_DRAG_FINISHED);
        }
    } else {
        evt.Skip();
    }
}

void MovableControl::OnMouseCaptureLost(wxMouseCaptureLostEvent& event)
{
    if (mDragging) {
        DragFinished();
    }
}

void MovableControl::DragFinished()
{
    if (auto parent = GetParent()) {
        wxWindowUpdateLocker freeze(this);
        ProcessDragEvent(parent, EVT_MOVABLE_CONTROL_DRAG_FINISHED);
    }
    mDragging = false;
}

void MovableControl::OnMouseDown(wxMouseEvent& evt)
{
    if (mDragging) {
        DragFinished();
        return;
    }

    mSourceIndex = mTargetIndex = FindIndexInParent();
    if (mSourceIndex != -1) {
        CaptureMouse();
        ProcessDragEvent(GetParent(), EVT_MOVABLE_CONTROL_DRAG_STARTED);

        mInitialPosition = evt.GetPosition();
        mDragging=true;
    }
}

void MovableControl::OnMouseUp(wxMouseEvent& evt)
{
    if (!mDragging) {
        return;
    }

    ReleaseMouse();

    DragFinished();
}

void MovableControl::OnMove(wxMouseEvent& evt)
{
    if (!mDragging) {
        return;
    }

    auto parent = GetParent();
    if (!parent) {
        return;
    }

    wxPoint newPosition = wxGetMousePosition() - mInitialPosition;
    Move(GetParent()->ScreenToClient(newPosition));

    if (auto boxSizer = dynamic_cast<wxBoxSizer*>(parent->GetSizer())) {
        if (boxSizer->GetOrientation() == wxVERTICAL) {
            auto targetIndex = mSourceIndex;

            //assuming that items are ordered from top to bottom (i > j <=> y(i) > y(j))
            //compare wxSizerItem position with the current MovableControl position!
            if (GetPosition().y < boxSizer->GetItem(mSourceIndex)->GetPosition().y) {
                //moving up
                for (int i = 0; i < mSourceIndex; ++i) {
                    const auto item = boxSizer->GetItem(i);

                    if (GetRect().GetTop() <= item->GetPosition().y + item->GetSize().y / 2) {
                        targetIndex = i;
                        break;
                    }
                }
            } else {
                //moving down
                for (int i = static_cast<int>(boxSizer->GetItemCount()) - 1; i > mSourceIndex; --i) {
                    const auto item = boxSizer->GetItem(i);
                    if (GetRect().GetBottom() >= item->GetPosition().y + item->GetSize().y / 2) {
                        targetIndex = i;
                        break;
                    }
                }
            }

            if (targetIndex != mTargetIndex) {
                mTargetIndex = targetIndex;
                ProcessDragEvent(parent, EVT_MOVABLE_CONTROL_DRAG_POSITION);
            }
        }
    }
}
