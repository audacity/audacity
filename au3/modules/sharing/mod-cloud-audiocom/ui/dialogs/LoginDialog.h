#pragma once
#include "Observer.h"
#include "wxPanelWrapper.h"

namespace audacity::cloud::audiocom {
struct AuthStateChangedMessage;
}

class wxHyperlinkEvent;
class wxTextCtrl;

class LoginDialog final : public wxDialogWrapper
{
public:

    enum class Mode
    {
        Create,
        SignIn,
    };

    static bool SignIn(wxWindow* parent, Mode mode = Mode::SignIn);

private:

    LoginDialog(wxWindow* parent, wxWindowID id = wxID_ANY, Mode mode = Mode::SignIn);

    void LayoutControls();

    void OnSignIn(wxHyperlinkEvent&);
    void OnCreateAccount(wxHyperlinkEvent&);
    void OnContinue(wxCommandEvent&);
    void OnContinueWithGoogle(wxCommandEvent&);
    void OnContinueWithFacebook(wxCommandEvent&);
    void onUserCredentialsChange(wxCommandEvent&);

    void ContinueCreateAccount();
    void ContinueSignIn();

    void OnOAuthStateChanged(audacity::cloud::audiocom::AuthStateChangedMessage);

    static void ContinueAuthorize(std::string_view authClientId);

    const Mode mMode;
    wxTextCtrl* mEmail{};
    wxTextCtrl* mPassword{};
    wxButton* mContinue{};

    Observer::Subscription mOAuthStateSubscription;

    DECLARE_EVENT_TABLE()
};
