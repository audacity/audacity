/**********************************************************************

  Audacity: A Digital Audio Editor

  ErrorReportDialog.h

  Dmitry Vedenko

**********************************************************************/

#ifndef __AUDACITY_SENTRYERRORDIALOG__
#define __AUDACITY_SENTRYERRORDIALOG__

#include <memory>

#include <wx/defs.h>
#include <wx/msgdlg.h>

#include "wxPanelWrapper.h" // to inherit

namespace audacity {
namespace sentry {
class Report;
}
}

class wxTextCtrl;

//! A dialog, that has "Send", "Don't send" and help buttons.
/*! This dialog is used in place of error dialogs for Audacity errors
    when Sentry reporting is enabled.
*/
class ErrorReportDialog final : public wxDialogWrapper
{
public:
    ErrorReportDialog(
        wxWindow* parent, const TranslatableString& dlogTitle, const TranslatableString& message, const ManualPageID& helpUrl,
        const wxString& log, const bool modal);

    ~ErrorReportDialog();

private:
    void OnSend(wxCommandEvent& event);
    void OnDontSend(wxCommandEvent& event);

    void OnHelp(wxCommandEvent& event);

    std::unique_ptr<audacity::sentry::Report> mReport;

    ManualPageID mHelpUrl;

    wxTextCtrl* mCommentsControl { nullptr };

    bool mIsModal;

    DECLARE_EVENT_TABLE()
};

#endif // __AUDACITY_SENTRYERRORDIALOG__
