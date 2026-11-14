/**********************************************************************

  Audacity: A Digital Audio Editor

  wxTextCtrlWrapper.h

  (Extracted from TimerRecordDialog.h)

**********************************************************************/

#ifndef __AUDACITY_WXTEXTCTRLWRAPPER__
#define __AUDACITY_WXTEXTCTRLWRAPPER__

#include <wx/textctrl.h>

// MY: Class that inherits from the wxTextCtrl class.
// We override AcceptsFocusFromKeyboard in order to add
// the text controls to the Tab Order since wxTextCtrls
// with the wxTE_READONLY style are normally skipped.
class wxTextCtrlWrapper final : public wxTextCtrl
{
public:
    wxTextCtrlWrapper(wxWindow* parent, wxWindowID id,
                      const wxString& value = {},
                      const wxPoint& pos = wxDefaultPosition,
                      const wxSize& size = wxDefaultSize,
                      long style = 0,
                      const wxValidator& validator = wxDefaultValidator,
                      const wxString& name = wxTextCtrlNameStr)
        :  wxTextCtrl(parent, id, value, pos, size, style, validator, name)
    {
        mReadOnly = false;

        Bind(wxEVT_KEY_DOWN, [&](wxKeyEvent& event)
        {
            auto keyCode = event.GetKeyCode();
            if (mReadOnly) {
                if (keyCode >= WXK_SPACE || keyCode == WXK_DELETE || keyCode == WXK_BACK) {
                    event.Skip(false);
                    return;
                }
            }

            event.Skip();
        });
    }

    ~wxTextCtrlWrapper()
    {
    }

    virtual bool AcceptsFocusFromKeyboard() const override
    {
        return true;
    }

    bool IsReadOnly()
    {
        return mReadOnly;
    }

    void SetReadOnly(bool readonly = true)
    {
        mReadOnly = readonly;
    }

private:
    bool mReadOnly;
};

#endif // __AUDACITY_WXTEXTCTRLWRAPPER__
