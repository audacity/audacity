/**********************************************************************

  Audacity: A Digital Audio Editor

  AButton.cpp

  Dominic Mazzoni

*******************************************************************//**

\class AButton
\brief A wxButton with mouse-over behaviour.

  AButton is a custom button class for Audacity.  The main feature it
  supports that a wxButton does not is mouseovers.

  It uses an image for all of its states: up, over, down, and
  disabled, allowing any sort of customization you want.  Currently
  it does not support transparency effects, so the image musts be
  rectangular and opaque.

*//*******************************************************************/

#include "AButton.h"

#include "AColor.h"
#include "AllThemeResources.h"
#include "Theme.h"
#include "TrackArt.h"

#include <wx/setup.h> // for wxUSE_* macros

#include <wx/dcbuffer.h>
#include <wx/eventfilter.h>

//This is needed for tooltips
#include "Project.h"
#include "ProjectStatus.h"
#include "../ProjectWindowBase.h"
#include <wx/tooltip.h>

BEGIN_EVENT_TABLE(AButton, wxWindow)
EVT_MOUSE_EVENTS(AButton::OnMouseEvent)
EVT_MOUSE_CAPTURE_LOST(AButton::OnCaptureLost)
EVT_KEY_DOWN(AButton::OnKeyDown)
EVT_CHAR_HOOK(AButton::OnCharHook)
EVT_SET_FOCUS(AButton::OnSetFocus)
EVT_KILL_FOCUS(AButton::OnKillFocus)
EVT_PAINT(AButton::OnPaint)
EVT_SIZE(AButton::OnSize)
EVT_ERASE_BACKGROUND(AButton::OnErase)
END_EVENT_TABLE()

// LL:  An alternative to this might be to just use the wxEVT_KILL_FOCUS
//      or wxEVT_ACTIVATE events.
class AButton::Listener final : public wxEventFilter
{
public:
    Listener (AButton* button);
    ~Listener();

    int FilterEvent(wxEvent& event) override;

    void OnEvent();

private:
    AButton* mButton;
};

AButton::Listener::Listener (AButton* button)
    : mButton(button)
{
    wxEvtHandler::AddFilter(this);
}

AButton::Listener::~Listener ()
{
    wxEvtHandler::RemoveFilter(this);
}

void AButton::Listener::OnEvent()
{
    if (!mButton->IsDown()) {
        int idx = 0;
        // Ignore the event, consult key states.  One modifier key might
        // have gone up but another remained down.
        // Note that CMD (or CTRL) takes precedence over Shift if both are down
        // and alternates are defined for both
        // see also AButton::OnMouseEvent()
        if (wxGetKeyState(WXK_CONTROL) && mButton->HasAlternateImages(2)) {
            idx = 2;
        } else if (wxGetKeyState(WXK_SHIFT) && mButton->HasAlternateImages(1)) {
            idx = 1;
        }

        // Turn e.g. the "Play" button into a "Loop" button
        // or "Cut Preview" button
        mButton->SetAlternateIdx(idx);
    }
}

int AButton::Listener::FilterEvent(wxEvent& event)
{
    if (event.GetEventType() == wxEVT_KEY_DOWN
        || event.GetEventType() == wxEVT_KEY_UP) {
        OnEvent();
    } else if (event.GetEventType() == wxEVT_SET_FOCUS) {
        // A modal dialog might have eaten the modifier key-up with its own
        // filter before we saw it; this is adequate to fix the button image
        // when the dialog disappears.
        OnEvent();
    }
    return Event_Skip;
}

AButton::AButton(wxWindow* parent, wxWindowID id, const wxPoint& pos, const wxSize& size, bool toggle)
{
    Init(parent, id, pos, size, toggle);
}

AButton::AButton(wxWindow* parent,
                 wxWindowID id,
                 const wxPoint& pos,
                 const wxSize& size,
                 const wxImage& up,
                 const wxImage& over,
                 const wxImage& down,
                 const wxImage& overDown,
                 const wxImage& dis,
                 bool toggle)
{
    Init(parent, id, pos, size, toggle);

    SetAlternateImages(0, up, over, down, overDown, dis);

    SetMinSize(mImages[0][AButtonUp].GetSize());
    SetMaxSize(mImages[0][AButtonUp].GetSize());
}

AButton::~AButton()
{
    if (HasCapture()) {
        ReleaseMouse();
    }
}

void AButton::SetButtonType(Type type)
{
    if (mType != type) {
        mType = type;
        InvalidateBestSize();
        Refresh(false);
        PostSizeEventToParent();
    }
}

void AButton::SetFrameMid(int mid)
{
    if (mid == mFrameMid) {
        return;
    }
    mFrameMid = mid;

    if (mType == FrameButton) {
        InvalidateBestSize();
        Refresh(false);
        PostSizeEventToParent();
    }
}

void AButton::Init(wxWindow* parent, wxWindowID id, const wxPoint& pos, const wxSize& size, bool toggle)
{
    SetBackgroundStyle(wxBG_STYLE_PAINT);
    SetBackgroundColour(theTheme.Colour(clrMedium));

    // Bug in wxWidgets 2.8.12: by default pressing Enter on an AButton is interpreted as
    // a navigation event - move to next control. As a workaround, the style wxWANTS_CHARS
    // results in all characters being available in the OnKeyDown function below. Note
    // that OnKeyDown now has to handle navigation.
    Create(parent, id, pos, size, wxWANTS_CHARS);

    mToggle = toggle;

    mFocusRect = GetClientRect().Deflate(3, 3);

#if wxUSE_ACCESSIBILITY
    SetName(wxT(""));
    SetAccessible(safenew AButtonAx(this));
#endif
}

void AButton::UseDisabledAsDownHiliteImage(bool flag)
{
    mUseDisabledAsDownHiliteImage = flag;
}

void AButton::SetToolTip(const TranslatableString& toolTip)
{
    wxWindow::SetToolTip(toolTip.Stripped().Translation());
}

void AButton::SetLabel(const TranslatableString& toolTip)
{
    wxWindow::SetLabel(toolTip.Stripped().Translation());
    if (mType == FrameButton) {
        InvalidateBestSize();
    }
}

// This compensates for a but in wxWidgets 3.0.2 for mac:
// Couldn't set focus from keyboard when AcceptsFocus returns false;
// this bypasses that limitation
void AButton::SetFocusFromKbd()
{
    auto temp = TemporarilyAllowFocus();
    SetFocus();
}

void AButton::SetImages(const wxImage& up, const wxImage& over, const wxImage& down, const wxImage& overDown, const wxImage& dis)
{
    SetAlternateImages(0, up, over, down, overDown, dis);
}

void AButton::SetAlternateImages(unsigned idx,
                                 const wxImage& up,
                                 const wxImage& over,
                                 const wxImage& down,
                                 const wxImage& overDown,
                                 const wxImage& dis)
{
    if (1 + idx > mImages.size()) {
        mImages.resize(1 + idx);
    }
    mImages[idx][AButtonUp] = up;
    mImages[idx][AButtonOver] = over;
    mImages[idx][AButtonDown] = down;
    mImages[idx][AButtonOverDown] = overDown;
    mImages[idx][AButtonDis] = dis;
}

void AButton::SetIcon(const wxImage& icon)
{
    SetAlternateIcon(0, icon);
}

void AButton::SetIcon(AButtonState state, const wxImage& icon)
{
    SetAlternateIcon(0, state, icon);
}

void AButton::SetIcons(const wxImage& up, const wxImage& down, const wxImage& disabled)
{
    SetAlternateIcons(0, up, down, disabled);
}

void AButton::SetAlternateIcon(unsigned idx, const wxImage& icon)
{
    if (1 + idx > mIcons.size()) {
        mIcons.resize(1 + idx);
    }
    mIcons[idx][AButtonUp] = icon;
    mIcons[idx][AButtonOver] = mIcons[idx][AButtonDown]
                                   =mIcons[idx][AButtonDis] = mIcons[idx][AButtonOverDown] = wxNullImage;
    Refresh(false);
}

void AButton::SetAlternateIcon(unsigned idx, AButtonState state, const wxImage& icon)
{
    if (1 + idx > mIcons.size()) {
        mIcons.resize(1 + idx);
    }
    mIcons[idx][state] = icon;
    Refresh(false);
}

void AButton::SetAlternateIcons(unsigned idx, const wxImage& up, const wxImage& down, const wxImage& disabled)
{
    if (1 + idx > mIcons.size()) {
        mIcons.resize(1 + idx);
    }
    mIcons[idx][AButtonUp] = up;
    mIcons[idx][AButtonOver] = up;
    mIcons[idx][AButtonDown] = down;
    mIcons[idx][AButtonOverDown] = down;
    mIcons[idx][AButtonDis] = disabled;
    Refresh(false);
}

void AButton::SetAlternateIdx(unsigned idx)
{
    // If alternate-image-state is already correct then
    // nothing to do (saves repainting button).
    if (mAlternateIdx == idx) {
        return;
    }
    mAlternateIdx = idx;
    Refresh(false);
    PostSizeEventToParent();
}

void AButton::FollowModifierKeys()
{
    if (!mListener) {
        mListener = std::make_unique<Listener>(this);
    }
}

void AButton::SetFocusRect(const wxRect& r)
{
    mFocusRect = r;
    mForceFocusRect = true;
}

AButton::AButtonState AButton::GetState()
{
    AButtonState state;

    if (!mEnabled && (!mToggle || !mButtonIsDown)) {
        return AButtonDis;
    }

    if (mCursorIsInWindow) {
        if (mToggle) {
            if (mIsClicking) {
                state = mButtonIsDown ? AButtonUp : AButtonDown;
                if (mUseDisabledAsDownHiliteImage) {
                    state = mButtonIsDown ? AButtonOverDown : AButtonDis;
                }
            } else {
                state = mButtonIsDown ? AButtonOverDown : AButtonOver;
                if (mUseDisabledAsDownHiliteImage) {
                    state = mButtonIsDown ? AButtonDis : AButtonOver;
                }
            }
        } else {
            if (mIsClicking) {
                state = mButtonIsDown ? AButtonOver : AButtonDown;
            } else {
                state = mButtonIsDown ? AButtonOverDown : AButtonOver;
            }
        }
    } else {
        state = mButtonIsDown ? AButtonDown : AButtonUp;
    }

    return state;
}

void AButton::OnPaint(wxPaintEvent& WXUNUSED(event))
{
    wxBufferedPaintDC dc(this);

    dc.SetPen(*wxTRANSPARENT_PEN);
    dc.SetBrush(GetBackgroundColour());
    dc.Clear();

    const auto buttonRect = GetClientRect();
    const auto imageIdx = HasAlternateImages(mAlternateIdx) ? mAlternateIdx : 0;

    if (imageIdx == mAlternateIdx || HasAlternateImages(imageIdx)) {
        const auto buttonState = GetState();
        const auto isFrameTextButton = mType == FrameTextVButton || mType == FrameTextHButton;
        if (mType == ImageButton) {
            dc.DrawBitmap(mImages[imageIdx][buttonState], buttonRect.GetTopLeft());
        } else if (mType == FrameButton || isFrameTextButton) {
            wxBitmap bitmap = mImages[imageIdx][buttonState];
            AColor::DrawFrame(dc, buttonRect, bitmap, mFrameMid);

            const auto border = bitmap.GetSize() / 4;

            wxImage* icon{};
            if (mIcons.size() > mAlternateIdx) {
                icon = &mIcons[mAlternateIdx][buttonState];
            }
            if ((icon == nullptr || !icon->IsOk()) && !mIcons.empty()) {
                icon = &mIcons[0][buttonState];
                if (!icon->IsOk()) {
                    icon = &mIcons[0][AButtonUp];
                }
            }
            if (isFrameTextButton && !GetLabel().IsEmpty()) {
                dc.SetFont(GetFont());
                auto textRect = buttonRect;
                if (icon != nullptr && icon->IsOk()) {
                    const auto fontMetrics = dc.GetFontMetrics();
                    if (mType == FrameTextVButton) {
                        const auto sumHeight = fontMetrics.height + icon->GetHeight() + border.y;

                        dc.DrawBitmap(*icon,
                                      buttonRect.x + (buttonRect.width - icon->GetWidth()) / 2,
                                      buttonRect.y + (buttonRect.height - sumHeight) / 2);
                        textRect = wxRect(
                            buttonRect.x,
                            buttonRect.y + buttonRect.height / 2 + sumHeight / 2 - fontMetrics.height,
                            buttonRect.width,
                            fontMetrics.height);
                    } else {
                        const auto sumWidth = icon->GetWidth() + border.x + dc.GetTextExtent(GetLabel()).GetWidth();
                        const auto iconCenter = buttonRect.height / 2;
                        const auto textLeft = iconCenter + icon->GetWidth() / 2 + border.x;

                        dc.DrawBitmap(*icon,
                                      buttonRect.x + iconCenter - icon->GetWidth() / 2,
                                      buttonRect.y + iconCenter - icon->GetHeight() / 2);
                        textRect = wxRect(
                            buttonRect.x + textLeft,
                            buttonRect.y + border.y,
                            buttonRect.width - textLeft,
                            buttonRect.height - border.y * 2
                            );
                    }
                }
                dc.SetPen(GetForegroundColour());
                dc.DrawLabel(GetLabel(), textRect, wxALIGN_CENTER);
            } else if (icon != nullptr && icon->IsOk()) {
                dc.DrawBitmap(*icon,
                              buttonRect.x + (buttonRect.width - icon->GetWidth()) / 2,
                              buttonRect.y + (buttonRect.height - icon->GetHeight()) / 2);
            }
        } else {
            wxBitmap bitmap = mImages[imageIdx][buttonState];
            AColor::DrawHStretch(dc, GetClientRect(), bitmap);
            if (!GetLabel().IsEmpty()) {
                dc.SetFont(GetFont());
                const auto text = TrackArt::TruncateText(dc, GetLabel(), GetClientSize().GetWidth() - 6);
                if (!text.IsEmpty()) {
                    dc.SetPen(GetForegroundColour());
                    dc.DrawLabel(text, GetClientRect(), wxALIGN_CENTER);
                }
            }
        }
    }

    if (HasFocus()) {
        AColor::DrawFocus(dc, mFocusRect);
    }
}

void AButton::OnErase(wxEraseEvent& WXUNUSED(event))
{
    // Ignore it to prevent flashing
}

void AButton::OnSize(wxSizeEvent& WXUNUSED(event))
{
    if (!mForceFocusRect) {
        mFocusRect = GetClientRect().Deflate(3, 3);
    }
    Refresh(false);
}

bool AButton::s_AcceptsFocus{ false };

bool AButton::HasAlternateImages(unsigned idx) const
{
    if (mImages.size() <= idx) {
        return false;
    }

    const auto& arr = mImages[idx];
    return arr[0].Ok()
           && arr[1].Ok()
           && arr[2].Ok()
           && arr[3].Ok()
           && arr[4].Ok();
}

void AButton::OnMouseEvent(wxMouseEvent& event)
{
    wxSize clientSize = GetClientSize();
    AButtonState prevState = GetState();

    if (event.Entering()) {
        // Bug 1201:  On Mac, unsetting and re-setting the tooltip may be needed
        // to make it pop up when we want it.
        auto text = GetToolTipText();
        UnsetToolTip();
        wxWindow::SetToolTip(text);
        mCursorIsInWindow = true;
    } else if (event.Leaving()) {
        mCursorIsInWindow = false;
    } else {
        mCursorIsInWindow
            =(event.m_x >= 0 && event.m_y >= 0
              && event.m_x < clientSize.x && event.m_y < clientSize.y);
    }

    if (mEnabled && event.IsButton()) {
        if (event.ButtonIsDown(wxMOUSE_BTN_LEFT)) {
            mIsClicking = true;
            if (event.ButtonDClick()) {
                mIsDoubleClicked = true;
            }
            if (!HasCapture()) {
                CaptureMouse();
            }
        } else if (mIsClicking) {
            mIsClicking = false;

            if (HasCapture()) {
                ReleaseMouse();
            }

            if (mCursorIsInWindow && (mToggle || !mButtonIsDown)) {
                if (mToggle) {
                    mButtonIsDown = !mButtonIsDown;
                }

                mWasShiftDown = event.ShiftDown();
                mWasControlDown = event.ControlDown();

                Click();
            }
        }
    }

    // Only redraw and change tooltips if the state has changed.
    AButtonState newState = GetState();

    if (newState != prevState) {
        Refresh(false);

        if (mCursorIsInWindow) {
            UpdateStatus();
        } else {
            auto pProject = FindProjectFromWindow(this);
            if (pProject) {
                ProjectStatus::Get(*pProject).Set({});
            }
        }
    } else {
        event.Skip();
    }
}

void AButton::UpdateStatus()
{
    if (mCursorIsInWindow) {
#if wxUSE_TOOLTIPS // Not available in wxX11
        // Display the tooltip in the status bar
        wxToolTip* pTip = this->GetToolTip();
        if (pTip) {
            auto tipText = Verbatim(pTip->GetTip());
            if (!mEnabled) {
                tipText.Join(XO("(disabled)"), " ");
            }
            auto pProject = FindProjectFromWindow(this);
            if (pProject) {
                ProjectStatus::Get(*pProject).Set(tipText);
            }
        }
#endif
    }
}

void AButton::OnCaptureLost(wxMouseCaptureLostEvent& WXUNUSED(event))
{
    wxMouseEvent e(wxEVT_LEFT_UP);
    e.m_x = -1;
    e.m_y = -1;
    OnMouseEvent(e);
}

// Note that OnKeyDown has to handle navigation because wxWANTS_CHARS
// flag was set - see above.
void AButton::OnKeyDown(wxKeyEvent& event)
{
    switch (event.GetKeyCode()) {
    case WXK_RIGHT:
    case WXK_DOWN:
        Navigate(wxNavigationKeyEvent::IsForward);
        break;
    case WXK_LEFT:
    case WXK_UP:
        Navigate(wxNavigationKeyEvent::IsBackward);
        break;
    case WXK_TAB:
        Navigate(wxNavigationKeyEvent::FromTab | (event.ShiftDown()
                                                  ? wxNavigationKeyEvent::IsBackward
                                                  : wxNavigationKeyEvent::IsForward));
        break;
    default:
        event.Skip();
    }
}

void AButton::OnCharHook(wxKeyEvent& event)
{
    switch (event.GetKeyCode()) {
    case WXK_RETURN:
    case WXK_NUMPAD_ENTER:
        if (!mEnabled) {
            break;
        }
        mWasShiftDown = event.ShiftDown();
        mWasControlDown = event.ControlDown();
        if (mToggle) {
            mButtonIsDown = !mButtonIsDown;
            Refresh(false);
#if wxUSE_ACCESSIBILITY
            GetAccessible()->NotifyEvent(wxACC_EVENT_OBJECT_NAMECHANGE,
                                         this, wxOBJID_CLIENT, wxACC_SELF);
#endif
        }
        Click();
        break;
    default:
        event.Skip();
    }
}

void AButton::OnSetFocus(wxFocusEvent& WXUNUSED(event))
{
    Refresh(false);
}

void AButton::OnKillFocus(wxFocusEvent& WXUNUSED(event))
{
    Refresh(false);
}

bool AButton::WasShiftDown()
{
    return mWasShiftDown;
}

bool AButton::WasControlDown()
{
    return mWasControlDown;
}

void AButton::Enable()
{
    bool changed = wxWindow::Enable(true);
    if (!mEnabled) {
        mEnabled = true;
        Refresh(false);
    }
}

void AButton::Disable()
{
    // Bug 1565: Tooltips not showing on disabled buttons.
    // The fix is to NOT tell windows that the button is disabled.
    // The button's appearance will still change to show it is disabled
    // since we control that rather than windows.
#ifndef __WXMSW__
    wxWindow::Enable(false);
#endif
    if (GetCapture() == this) {
        ReleaseMouse();
    }
    if (mEnabled) {
        mEnabled = false;
        Refresh(false);
    }
}

void AButton::PushDown()
{
    if (!mButtonIsDown) {
        mButtonIsDown = true;
        this->Refresh(false);
    }
}

void AButton::PopUp()
{
    if (mButtonIsDown) {
        mButtonIsDown = false;

        this->Refresh(false);
    }

    if (GetCapture() == this) {
        ReleaseMouse();
    }
}

void AButton::Click()
{
    wxCommandEvent event(wxEVT_COMMAND_BUTTON_CLICKED, GetId());
    event.SetEventObject(this);
    // Be sure to use SafelyProcessEvent so that exceptions do not propagate
    // out of DoDefaultAction
    GetEventHandler()->SafelyProcessEvent(event);
}

void AButton::SetShift(bool shift)
{
    mWasShiftDown = shift;
}

void AButton::SetControl(bool control)
{
    mWasControlDown = control;
}

wxSize AButton::DoGetBestClientSize() const
{
    const auto imageIdx = HasAlternateImages(mAlternateIdx) ? mAlternateIdx : 0;
    if (imageIdx == mAlternateIdx || HasAlternateImages(imageIdx)) {
        const auto& image = mImages[imageIdx][AButtonUp];
        switch (mType) {
        case FrameButton:
        {
            const auto icon = !mIcons.empty() ? &mIcons[0][AButtonUp] : nullptr;
            if (icon->IsOk()) {
                return icon->GetSize();
            }
            return image.GetSize();
        }
        case FrameTextVButton:
        case FrameTextHButton:
        {
            //Only AButtonUp is used to estimate size
            auto icon = !mIcons.empty() ? &mIcons[0][AButtonUp] : nullptr;
            if (!GetLabel().IsEmpty()) {
                const auto border = (image.GetSize() - wxSize { mFrameMid, mFrameMid }) / 4;

                wxMemoryDC dc;
                dc.SetFont(GetFont());
                auto bestSize = dc.GetTextExtent(GetLabel());
                if (icon != nullptr && icon->IsOk()) {
                    if (mType == FrameTextVButton) {
                        bestSize.x = std::max(bestSize.x, icon->GetWidth());
                        bestSize.y = bestSize.y > 0
                                     ? bestSize.y + border.y + icon->GetHeight()
                                     : icon->GetHeight();
                    } else {
                        bestSize.x += image.GetWidth() + border.x;
                        bestSize.y = std::max(image.GetHeight(), bestSize.y);
                    }
                }
                if (bestSize.x > 0) {
                    bestSize.x += border.x * 2;
                }
                if (bestSize.y > 0) {
                    bestSize.y += border.y * 2;
                }
                return bestSize;
            }
            if (icon->Ok()) {
                return icon->GetSize();
            }
            return image.GetSize();
        }
        case TextButton:
            return { -1, image.GetHeight() };
        default:
            return image.GetSize();
        }
    }
    return wxWindow::DoGetBestClientSize();
}

auto AButton::TemporarilyAllowFocus() -> TempAllowFocus
{
    s_AcceptsFocus = true;
    return TempAllowFocus{ &s_AcceptsFocus };
}

#if wxUSE_ACCESSIBILITY

AButtonAx::AButtonAx(wxWindow* window)
    : WindowAccessible(window)
{
}

AButtonAx::~AButtonAx()
{
}

// Performs the default action. childId is 0 (the action for this object)
// or > 0 (the action for a child).
// Return wxACC_NOT_SUPPORTED if there is no default action for this
// window (e.g. an edit control).
wxAccStatus AButtonAx::DoDefaultAction(int WXUNUSED(childId))
{
    AButton* ab = wxDynamicCast(GetWindow(), AButton);

    if (ab && ab->IsEnabled()) {
        ab->mWasShiftDown = false;
        ab->mWasControlDown = false;
        if (ab->mToggle) {
            ab->mButtonIsDown = !ab->mButtonIsDown;
            ab->Refresh(false);
        }
        ab->Click();
    }

    return wxACC_OK;
}

// Retrieves the address of an IDispatch interface for the specified child.
// All objects must support this property.
wxAccStatus AButtonAx::GetChild(int childId, wxAccessible** child)
{
    if (childId == wxACC_SELF) {
        *child = this;
    } else {
        *child = NULL;
    }

    return wxACC_OK;
}

// Gets the number of children.
wxAccStatus AButtonAx::GetChildCount(int* childCount)
{
    *childCount = 0;

    return wxACC_OK;
}

// Gets the default action for this object (0) or > 0 (the action for
// a child).  Return wxACC_OK even if there is no action. actionName
// is the action, or the empty string if there is no action.  The
// retrieved string describes the action that is performed on an
// object, not what the object does as a result. For example, a
// toolbar button that prints a document has a default action of
// "Press" rather than "Prints the current document."
wxAccStatus AButtonAx::GetDefaultAction(int WXUNUSED(childId), wxString* actionName)
{
    *actionName = _("Press");

    return wxACC_OK;
}

// Returns the description for this object or a child.
wxAccStatus AButtonAx::GetDescription(int WXUNUSED(childId), wxString* description)
{
    description->clear();

    return wxACC_OK;
}

// Gets the window with the keyboard focus.
// If childId is 0 and child is NULL, no object in
// this subhierarchy has the focus.
// If this object has the focus, child should be 'this'.
wxAccStatus AButtonAx::GetFocus(int* childId, wxAccessible** child)
{
    *childId = 0;
    *child = this;

    return wxACC_OK;
}

// Returns help text for this object or a child, similar to tooltip text.
wxAccStatus AButtonAx::GetHelpText(int WXUNUSED(childId), wxString* helpText)
{
#if wxUSE_TOOLTIPS // Not available in wxX11
    AButton* ab = wxDynamicCast(GetWindow(), AButton);

    wxToolTip* pTip = ab->GetToolTip();
    if (pTip) {
        *helpText = pTip->GetTip();
    }

    return wxACC_OK;
#else
    helpText->clear();

    return wxACC_NOT_SUPPORTED;
#endif
}

// Returns the keyboard shortcut for this object or child.
// Return e.g. ALT+K
wxAccStatus AButtonAx::GetKeyboardShortcut(int WXUNUSED(childId), wxString* shortcut)
{
    shortcut->clear();

    return wxACC_OK;
}

// Returns the rectangle for this object (id = 0) or a child element (id > 0).
// rect is in screen coordinates.
wxAccStatus AButtonAx::GetLocation(wxRect& rect, int WXUNUSED(elementId))
{
    AButton* ab = wxDynamicCast(GetWindow(), AButton);

    rect = ab->GetRect();
    rect.SetPosition(ab->GetParent()->ClientToScreen(rect.GetPosition()));

    return wxACC_OK;
}

// Gets the name of the specified object.
wxAccStatus AButtonAx::GetName(int WXUNUSED(childId), wxString* name)
{
    AButton* ab = wxDynamicCast(GetWindow(), AButton);

    *name = ab->GetName();
    if (name->empty()) {
        *name = ab->GetLabel();
    }

    if (name->empty()) {
        *name = _("Button");
    }

    /* In the MSAA frame work, there isn't such a thing as a toggle button.
    In particular, narrator does not read the wxACC_STATE_SYSTEM_PRESSED state at all.
    So to imitate a toggle button, include the role and the state in the name, and
    create a name change event when the state changes. To enable screen reader
    scripts to determine the state of the toggle button in the absence of the
    accessibility state indicating this, add the '\a' character to the end of the name
    when the button is pressed. ('\a' is read silently by screen readers.) */
    if (ab->mToggle) {
        *name += wxT(" ")
                 + _("Button")
                 + wxT(" ")
                 +/* i18n-hint: whether a button is pressed or not pressed */
                 (ab->IsDown() ? _("pressed") + wxT('\a') : _("not pressed"));
    }

    return wxACC_OK;
}

// Returns a role constant.
wxAccStatus AButtonAx::GetRole(int WXUNUSED(childId), wxAccRole* role)
{
    AButton* ab = wxDynamicCast(GetWindow(), AButton);

    // For a toggle button, the role is included in the name, so read nothing
    *role = ab->mToggle ? wxROLE_SYSTEM_STATICTEXT : wxROLE_SYSTEM_PUSHBUTTON;

    return wxACC_OK;
}

// Gets a variant representing the selected children
// of this object.
// Acceptable values:
// - a null variant (IsNull() returns TRUE)
// - a list variant (GetType() == wxT("list"))
// - an integer representing the selected child element,
//   or 0 if this object is selected (GetType() == wxT("long"))
// - a "void*" pointer to a wxAccessible child object
wxAccStatus AButtonAx::GetSelections(wxVariant* WXUNUSED(selections))
{
    return wxACC_NOT_IMPLEMENTED;
}

// Returns a state constant.
wxAccStatus AButtonAx::GetState(int WXUNUSED(childId), long* state)
{
    AButton* ab = wxDynamicCast(GetWindow(), AButton);
    *state = 0;
    if (!ab->IsEnabled()) {
        *state = wxACC_STATE_SYSTEM_UNAVAILABLE;
    } else {
        // For a toggle button, the state is included in the name
        if (ab->mButtonIsDown && !ab->mToggle) {
            *state |= wxACC_STATE_SYSTEM_PRESSED;
        }

        if (ab->mCursorIsInWindow) {
            *state |= wxACC_STATE_SYSTEM_HOTTRACKED;
        }

        *state |= wxACC_STATE_SYSTEM_FOCUSABLE;
        if (ab->HasFocus()) {
            *state |= wxACC_STATE_SYSTEM_FOCUSED;
        }
    }
    return wxACC_OK;
}

// Returns a localized string representing the value for the object
// or child.
wxAccStatus AButtonAx::GetValue(int WXUNUSED(childId), wxString* WXUNUSED(strValue))
{
    return wxACC_NOT_SUPPORTED;
}

#endif
