#include "LoginDialog.h"

#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/button.h>
#include <wx/textctrl.h>
#include <wx/hyperlink.h>
#include <wx/statbmp.h>

#include "Theme.h"
#include "AllThemeResources.h"
#include "BasicUI.h"
#include "ExportUtils.h"
#include "OAuthService.h"
#include "widgets/AButton.h"

namespace {
constexpr auto RestorePasswordURL = "https://audio.com/auth/forgot-password";

enum
{
    ID_SIGNIN,
    ID_CREATE_ACCOUNT,
    ID_WITH_GOOGLE,
    ID_WITH_FACEBOOK
};

auto DialogNameForMode(LoginDialog::Mode mode)
{
    return mode == LoginDialog::Mode::Create
           ? XO("Create cloud account")
           : XO("Sign in to cloud");
}

AButton* MakeLoginButton(wxWindow* parent, wxWindowID id, int imageId, const TranslatableString& label)
{
    const auto button = safenew AButton(parent, id);
    button->SetButtonType(AButton::FrameTextHButton);
    button->SetImages(
        theTheme.Image(bmpHButtonNormal),
        theTheme.Image(bmpHButtonHover),
        theTheme.Image(bmpHButtonDown),
        theTheme.Image(bmpHButtonHover),
        theTheme.Image(bmpHButtonDisabled));
    button->SetFrameMid(2);
    button->SetIcon(theTheme.Image(imageId));
    button->SetLabel(label);
    button->SetMinSize({ 206, 40 });
    button->SetSize(206, 40);
    button->SetForegroundColour(theTheme.Colour(clrTrackPanelText));
    return button;
}

wxStaticText* MakeLabel(wxWindow* parent, const wxString& text)
{
    auto label = safenew wxStaticText(parent, wxID_ANY, text);
    label->SetForegroundColour(theTheme.Colour(clrTrackPanelText));
    return label;
}

auto DefaultErrorHandler(LoginDialog* dialog, LoginDialog::Mode mode)
{
    return [weak = wxWeakRef(dialog), mode](auto code, auto error)
    {
        BasicUI::CallAfter([weak, mode, code, error = std::string(error)]
        {
            using namespace BasicUI;
            if (mode == LoginDialog::Mode::Create) {
                ShowMessageBox(XXO("Oops! It looks like there was an issue with your input.\n"
                                   "Please ensure your email is correct and "
                                   "that your password is at least 8 characters long"),
                               MessageBoxOptions {}.IconStyle(Icon::Error));
            } else {
                ShowMessageBox(XXO("Sorry, but it seems the email or password you entered is incorrect.\n"
                                   "Please double-check your credentials and try again!"),
                               MessageBoxOptions {}.IconStyle(Icon::Error));
            }
            if (weak) {
                weak->Enable();
            }
        });
    };
}
}

BEGIN_EVENT_TABLE(LoginDialog, wxDialogWrapper)
EVT_HYPERLINK(ID_SIGNIN, LoginDialog::OnSignIn)
EVT_HYPERLINK(ID_CREATE_ACCOUNT, LoginDialog::OnCreateAccount)
EVT_BUTTON(wxID_OK, LoginDialog::OnContinue)
EVT_BUTTON(ID_WITH_GOOGLE, LoginDialog::OnContinueWithGoogle)
EVT_BUTTON(ID_WITH_FACEBOOK, LoginDialog::OnContinueWithFacebook)
END_EVENT_TABLE()

LoginDialog::LoginDialog(wxWindow* parent, wxWindowID id, Mode mode)
    : wxDialogWrapper(parent, id, DialogNameForMode(mode))
    , mMode(mode)
{
    mOAuthStateSubscription
        =audacity::cloud::audiocom::GetOAuthService().Subscribe(*this, &LoginDialog::OnOAuthStateChanged);
#if defined(__WXMSW__)
    SetBackgroundColour(theTheme.Colour(clrMedium));
#endif

    LayoutControls();
}

bool LoginDialog::SignIn(wxWindow* parent, Mode mode)
{
    while (true)
    {
        LoginDialog dialog(parent, wxID_ANY, mode);
        dialog.wxDialogWrapper::Center();
        auto result = dialog.wxDialogWrapper::ShowModal();
        if (result == ID_SIGNIN) {
            mode = Mode::SignIn;
        } else if (result == ID_CREATE_ACCOUNT) {
            mode = Mode::Create;
        } else {
            return result == wxID_OK;
        }
    }
}

void LoginDialog::LayoutControls()
{
    wxFont titleFont = GetFont();
    titleFont.SetWeight(wxFONTWEIGHT_BOLD);
    titleFont.SetPixelSize({ 0, 18 });

    auto topSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);
    wxStaticText* title;
    topSizer->Add(
        title = MakeLabel(this,
                          mMode == Mode::Create
                          ? _("Create an account to save to the cloud")
                          : _("Sign in to save to the cloud")),
        0, wxALL, 16);
    title->SetFont(titleFont);
    {
        auto hSizer = std::make_unique<wxBoxSizer>(wxHORIZONTAL);
        hSizer->Add(MakeLoginButton(this, ID_WITH_GOOGLE, bmpGoogleLogo, XO("Continue with Google")), 1, wxEXPAND);
        hSizer->AddSpacer(8);
        hSizer->Add(MakeLoginButton(this, ID_WITH_FACEBOOK, bmpFacebookLogo, XO("Continue with Facebook")), 1, wxEXPAND);

        topSizer->Add(hSizer.release(), 0, wxEXPAND | wxLEFT | wxRIGHT | wxBOTTOM, 16);
    }

    {
        auto vSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);
        vSizer->Add(MakeLabel(this, _("Email")), 0, wxBOTTOM, 5);
        vSizer->Add(mEmail = safenew wxTextCtrl(this, wxID_ANY), 0, wxEXPAND);
        topSizer->Add(vSizer.release(), 0, wxEXPAND | wxLEFT | wxRIGHT | wxBOTTOM, 16);
    }
    {
        auto vSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);
        vSizer->Add(MakeLabel(this, _("Password")), 0, wxBOTTOM, 5);
        vSizer->Add(mPassword
                        =safenew wxTextCtrl(
                              this,
                              wxID_ANY,
                              wxEmptyString,
                              wxDefaultPosition,
                              wxDefaultSize,
                              wxTE_PASSWORD),
                    0, wxEXPAND);
        if (mMode == Mode::SignIn) {
            vSizer->Add(safenew wxHyperlinkCtrl(this, wxID_ANY, _("Forgot your password?"), RestorePasswordURL), 0, wxTOP, 10);
        }

        topSizer->Add(vSizer.release(), 0, wxEXPAND | wxLEFT | wxRIGHT | wxBOTTOM, 16);

        mEmail->Bind(wxEVT_TEXT, &LoginDialog::onUserCredentialsChange, this);
        mPassword->Bind(wxEVT_TEXT, &LoginDialog::onUserCredentialsChange, this);
    }
    if (mMode == Mode::Create) {
        auto hSizer = std::make_unique<wxBoxSizer>(wxHORIZONTAL);
        //i18b-hint: followed by hyperlink, keep whitespace at the end
        hSizer->Add(MakeLabel(this, _("Already have an account? ")), 0, wxALIGN_CENTER_VERTICAL);
        hSizer->Add(safenew wxHyperlinkCtrl(this, ID_SIGNIN, _("Sign in here"), ""));
        topSizer->Add(hSizer.release(), 0, wxEXPAND | wxLEFT | wxRIGHT, 16);
    } else {
        auto hSizer = std::make_unique<wxBoxSizer>(wxHORIZONTAL);
        //i18b-hint: followed by hyperlink, keep whitespace at the end
        hSizer->Add(MakeLabel(this, _("Need an account? ")), 0, wxALIGN_CENTER_VERTICAL);
        hSizer->Add(safenew wxHyperlinkCtrl(this, ID_CREATE_ACCOUNT, _("Create cloud account"), ""));
        topSizer->Add(hSizer.release(), 0, wxEXPAND | wxALL, 16);
    }

    {
        auto hSizer = std::make_unique<wxBoxSizer>(wxHORIZONTAL);
        hSizer->Add(safenew wxButton(this, wxID_CANCEL, _("Cancel")));
        hSizer->AddSpacer(8);
        hSizer->Add(mContinue = safenew wxButton(this, wxID_OK, _("Continue")));
        topSizer->Add(hSizer.release(), 0, wxALIGN_RIGHT | wxALL, 16);
        mContinue->Disable();
    }

    mEmail->SetFocus();

    SetSizerAndFit(topSizer.release());
}

void LoginDialog::OnSignIn(wxHyperlinkEvent&)
{
    EndModal(ID_SIGNIN);
}

void LoginDialog::OnCreateAccount(wxHyperlinkEvent&)
{
    EndModal(ID_CREATE_ACCOUNT);
}

void LoginDialog::OnContinue(wxCommandEvent&)
{
    if (mMode == Mode::Create) {
        ContinueCreateAccount();
    } else {
        ContinueSignIn();
    }
}

void LoginDialog::OnContinueWithGoogle(wxCommandEvent&)
{
    ContinueAuthorize("google");
}

void LoginDialog::OnContinueWithFacebook(wxCommandEvent&)
{
    ContinueAuthorize("facebook");
}

void LoginDialog::onUserCredentialsChange(wxCommandEvent&)
{
    if (mEmail->IsEmpty() || mPassword->IsEmpty()) {
        mContinue->Disable();
    } else {
        mContinue->Enable();
    }
}

void LoginDialog::ContinueCreateAccount()
{
    Disable();

    const auto email = mEmail->GetValue().ToStdString();
    const auto password = mPassword->GetValue().ToStdString();

    auto& oauthService = audacity::cloud::audiocom::GetOAuthService();
    oauthService.Register(
        email,
        password,
        [weakThis = wxWeakRef(this)](auto token)
    {
        BasicUI::CallAfter([weakThis, token = std::string(token)]()
        {
            if (!weakThis) {
                return;
            }

            if (token.empty()) {
                weakThis->Enable();
            } else {
                weakThis->EndModal(wxID_OK);
            }
        });
    },
        DefaultErrorHandler(this, mMode),
        AudiocomTrace::ignore);
}

void LoginDialog::ContinueSignIn()
{
    Disable();

    const auto email = mEmail->GetValue().ToStdString();
    const auto password = mPassword->GetValue().ToStdString();

    auto& oauthService = audacity::cloud::audiocom::GetOAuthService();
    oauthService.Authorize(email, password,
                           [weakThis = wxWeakRef(this)](auto token)
    {
        BasicUI::CallAfter([weakThis, token = std::string(token)]()
        {
            if (!weakThis) {
                return;
            }

            if (token.empty()) {
                weakThis->Enable();
            } else {
                weakThis->EndModal(wxID_OK);
            }
        });
    },
                           DefaultErrorHandler(this, mMode),
                           AudiocomTrace::ignore);
}

void LoginDialog::OnOAuthStateChanged(audacity::cloud::audiocom::AuthStateChangedMessage message)
{
    if (message.authorised) {
        EndModal(wxOK);
    }
}

void LoginDialog::ContinueAuthorize(std::string_view authClientId)
{
    BasicUI::OpenInDefaultBrowser(
        audacity::cloud::audiocom::OAuthService::MakeOAuthRequestURL(authClientId)
        );
}
