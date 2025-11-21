/**********************************************************************

  Audacity: A Digital Audio Editor

  ReadOnlyText.h

**********************************************************************/

#ifndef __AUDACITY_READONLYTEXT__
#define __AUDACITY_READONLYTEXT__

#include <wx/wx.h>

#include "WindowAccessible.h"

#if wxUSE_ACCESSIBILITY

class ReadOnlyTextAx final : public WindowAccessible
{
public:
    ReadOnlyTextAx(wxWindow* window)
        :  WindowAccessible(window)
    {
    }

    ~ReadOnlyTextAx()
    {
    }

    wxAccStatus GetRole(int childId, wxAccRole* role)
    {
        *role = wxROLE_SYSTEM_STATICTEXT;

        return wxACC_OK;
    }

    wxAccStatus GetState(int childId, long* state)
    {
        auto w = GetWindow();
        *state = wxACC_STATE_SYSTEM_FOCUSABLE | wxACC_STATE_SYSTEM_READONLY;
        *state |= (w == wxWindow::FindFocus() ? wxACC_STATE_SYSTEM_FOCUSED : 0);

        return wxACC_OK;
    }

    wxAccStatus GetValue(int childId, wxString* strValue)
    {
        *strValue = GetWindow()->GetLabel();

        return wxACC_OK;
    }
};

#endif

class ReadOnlyText final : public wxControl
{
public:
    ReadOnlyText(wxWindow* parent,
                 wxWindowID id,
                 const wxString& value,
                 const wxPoint& pos = wxDefaultPosition,
                 const wxSize& size = wxDefaultSize,
                 long style = wxBORDER_NONE)
        :  wxControl(parent, id, pos, size, style)
    {
#if wxUSE_ACCESSIBILITY
        SetAccessible(safenew ReadOnlyTextAx(this));
#endif
        SetInitialSize(size);

        Bind(wxEVT_SET_FOCUS, [&](wxFocusEvent& event)
        {
            SetForegroundColour(wxSystemSettings::GetColour(wxSYS_COLOUR_HIGHLIGHTTEXT));
            SetBackgroundColour(wxSystemSettings::GetColour(wxSYS_COLOUR_HIGHLIGHT));
            Refresh();
            event.Skip();
        });

        Bind(wxEVT_KILL_FOCUS, [&](wxFocusEvent& event)
        {
            SetForegroundColour(wxSystemSettings::GetColour(wxSYS_COLOUR_BTNTEXT));
            SetBackgroundColour(wxSystemSettings::GetColour(wxSYS_COLOUR_BTNFACE));
            Refresh();
            event.Skip();
        });

        Bind(wxEVT_PAINT, [&](wxPaintEvent& WXUNUSED(event))
        {
            wxPaintDC dc(this);

            wxRect rect = GetClientRect();
            if (!IsEnabled()) {
                // draw shadow of the text
                dc.SetTextForeground(wxSystemSettings::GetColour(wxSYS_COLOUR_BTNHIGHLIGHT));
                wxRect rectShadow = rect;
                rectShadow.Offset(1, 1);
                dc.DrawLabel(GetLabel(), rectShadow, GetAlignment());
                dc.SetTextForeground(wxSystemSettings::GetColour(wxSYS_COLOUR_BTNSHADOW));
            }
            dc.DrawLabel(GetLabel(), rect, GetAlignment());
        });
    }

    ~ReadOnlyText()
    {
    }

    wxSize DoGetBestClientSize() const override
    {
        wxClientDC dc(wxConstCast(this, ReadOnlyText));

        return dc.GetMultiLineTextExtent(GetLabel());
    }

    wxString GetValue()
    {
        return GetLabel();
    }

    void SetValue(const wxString& value)
    {
        SetLabel(value);
        Refresh();
    }

    void SetValue(const TranslatableString& value)
    {
        SetValue(value.Translation());
    }
};

#endif // __AUDACITY_READONLYTEXT__
