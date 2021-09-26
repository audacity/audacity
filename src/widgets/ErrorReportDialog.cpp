/**********************************************************************

  Audacity: A Digital Audio Editor

  ErrorReportDialog.cpp

  Dmitry Vedenko

**********************************************************************/

#include "ErrorReportDialog.h"

#include <wx/app.h>
#include <wx/artprov.h>
#include <wx/button.h>
#include <wx/collpane.h>
#include <wx/dialog.h>
#include <wx/html/htmlwin.h>
#include <wx/icon.h>
#include <wx/intl.h>
#include <wx/settings.h>
#include <wx/sizer.h>
#include <wx/statbmp.h>
#include <wx/stattext.h>
#include <wx/statusbr.h>
#include <wx/textctrl.h>
#include <wx/bmpbuttn.h>

#include "ui/AccessibleLinksFormatter.h"
#include "AllThemeResources.h"
#include "Theme.h"
#include "HelpText.h"
#include "Prefs.h"
#include "ShuttleGui.h"
#include "HelpSystem.h"

#include "SentryReport.h"
#include "CodeConversions.h"

constexpr int MaxUserCommentLength = 2000;
constexpr bool ErrorReportDialogHasUserComment = false;

BEGIN_EVENT_TABLE(ErrorReportDialog, wxDialogWrapper)
    EVT_BUTTON(wxID_YES, ErrorReportDialog::OnSend)
    EVT_BUTTON(wxID_NO, ErrorReportDialog::OnDontSend)
    EVT_BUTTON(wxID_HELP, ErrorReportDialog::OnHelp)
END_EVENT_TABLE()

ErrorReportDialog::ErrorReportDialog(
   wxWindow* parent, const TranslatableString& dlogTitle,
   const TranslatableString& message, const ManualPageID& helpUrl,
   const wxString& log, const bool modal)
    : wxDialogWrapper(
         parent, wxID_ANY, dlogTitle, wxDefaultPosition, wxDefaultSize,
         wxDEFAULT_DIALOG_STYLE)
    , mHelpUrl(helpUrl)
    , mIsModal(modal)
{
   audacity::sentry::Exception ex = audacity::sentry::Exception::Create(
      audacity::ToUTF8(dlogTitle.Debug()),message.Debug());

   if (!log.empty())
      ex.AddData("log", log);

   mReport = std::make_unique<audacity::sentry::Report> (ex);

   ShuttleGui S(this, eIsCreating);

   const wxFont headingFont = wxFont(wxFontInfo(12).Bold());
   const wxFont textFont = wxFont(wxFontInfo(10));

   S.SetBorder(0);

   S.StartHorizontalLay(wxEXPAND, 0);
   {
      S.AddSpace(40, 0);
      S.StartVerticalLay(wxEXPAND, 0);
      {
         S.AddSpace(0, 32);

         S.StartHorizontalLay(wxEXPAND, 1);
         {
            S.StartVerticalLay(0);
            {
               wxBitmap bitmap = wxArtProvider::GetBitmap(
                  wxART_WARNING, wxART_MESSAGE_BOX, wxSize(24, 24));

               S.Prop(0).AddWindow(
                  safenew wxStaticBitmap(S.GetParent(), -1, bitmap));

               S.AddSpace(0, 0, 1);
            }
            S.EndVerticalLay();

            S.AddSpace(10, 0);

            S.StartVerticalLay(0);
            {
               S.AddSpace(0, 7);

               S.Prop(1)
                  .AddVariableText(message, false, 0, 560)
                  ->SetFont(headingFont);
            }
            S.EndVerticalLay();
         }
         S.EndHorizontalLay();

         S.AddSpace(0, 20);

         S.AddVariableText(XO(
               "Click \"Send\" to submit the report to Audacity. This information is collected anonymously."))
            ->SetFont(textFont);

         S.AddSpace(0, 20);

         S.AddVariableText(XO("Problem details"))->SetFont(textFont);

         S.AddSpace(0, 6);

         S.Style(wxTE_RICH | wxTE_READONLY | wxTE_MULTILINE | wxTE_DONTWRAP)
            .MinSize(wxSize(0, 152))
            .Name(XO("Problem details"))
            .AddTextBox({}, mReport->GetReportPreview(), 0);

         S.AddSpace(0, 20);

         if constexpr (ErrorReportDialogHasUserComment)
         {
            S.AddVariableText(XO("Comments"))->SetFont(textFont);

            S.AddSpace(0, 6);

            mCommentsControl = S.Style(wxTE_MULTILINE)
                                  .MinSize(wxSize(0, 76))
                                  .Name(XO("Comments"))
                                  .AddTextBox({}, {}, 0);

            mCommentsControl->SetMaxLength(MaxUserCommentLength);

            S.AddSpace(0, 20);
         }

         /* i18n-hint: %s will be replaced with "our Privacy Policy" */
         AccessibleLinksFormatter privacyPolicy(XO("See %s for more info."));

         privacyPolicy.FormatLink(
            /* i18n-hint: Title of hyperlink to the privacy policy. This is an
               object of "See". */
            wxT("%s"), XO("our Privacy Policy"),
            "https://www.audacityteam.org/about/desktop-privacy-notice/");

         privacyPolicy.Populate(S);

		 S.AddSpace(0, 20);

         S.StartHorizontalLay(wxEXPAND);
         {
            if (!mHelpUrl.empty())
            {
               wxBitmapButton* helpButton =
                  S.Id(wxID_HELP).AddBitmapButton(theTheme.Bitmap(bmpHelpIcon));
               // For screen readers
               helpButton->SetToolTip(XO("Help").Translation());
               helpButton->SetLabel(XO("Help").Translation());
            }
            
            S.AddSpace(0, 0, 1);

            S.Id(wxID_NO).AddButton(XC("&Don't send", "crash reporter button"));

            S.AddSpace(13, 0);

            S.Id(wxID_YES).AddButton(XC("&Send", "crash reporter button"));
         }
         S.EndHorizontalLay();

         S.AddSpace(0, 20);
      }
      S.EndVerticalLay();

      S.AddSpace(28, 0);
   }
   S.EndHorizontalLay();

   S.SetBorder(2);

   Layout();
   GetSizer()->Fit(this);
   SetMinSize(GetSize());
   Center();
}

ErrorReportDialog::~ErrorReportDialog()
{
}

void ErrorReportDialog::OnSend(wxCommandEvent& event)
{
   Disable();

   if (mCommentsControl != nullptr)
      mReport->AddUserComment(audacity::ToUTF8(mCommentsControl->GetValue()));

   mReport->Send(
      [this](int code, std::string body) { 
         CallAfter([this]() { 
             EndModal(true); 
         });
   });
}

void ErrorReportDialog::OnDontSend(wxCommandEvent& event)
{
   EndModal(true);
}

void ErrorReportDialog::OnHelp(wxCommandEvent& event)
{
   const auto &helpUrl = mHelpUrl.GET();
   if (helpUrl.StartsWith(wxT("innerlink:")))
   {
      HelpSystem::ShowHtmlText(
         this, TitleText(helpUrl.Mid(10)), HelpText(helpUrl.Mid(10)), false,
         true);
      return;
   }

   HelpSystem::ShowHelp(this, mHelpUrl, false);
}
