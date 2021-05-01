#include "TelemetryDialog.h"

#include <wx/sstream.h>
#include <wx/txtstrm.h>
#include <wx/fs_mem.h>
#include <wx/button.h>

#include "ShuttleGui.h"
#include "widgets/HelpSystem.h"

#include "lib-string-utils/CodeConversions.h"

#include "../../images/Telemetry/TelemetryEmojii.h"

static const auto title =
/* i18n-hint: Title of the telemetry permission dialog */
XO ("Help us improve Audacity");

static const auto firstParagraph =
/* i18-hint: The first paragraph of telemetry permission dialog */
XO (
"We would like to collect anonymous usage data to \
help us prioritize improvements and making Audacity \
better in the future. This includes things like \
frequency of use, preferred file formats, crash logs etc."
);

static const auto secondParagraph =
/* i18-hint: The second paragraph of telemetry permission dialog */
XO (
"We <u>do not</u> collect any personal data \
or sensitive information such as location or \
file names or any content of your audio. \
<a href=\"https://audacityteam.org\">Privacy policy</a>"
);

static const auto thirdParagraph =
/* i18-hint: Hint to the user about how to change the setting it telemetry permission dialog */
XO (
    "You can change this at any time in <b>Preferences > Analytics</b>."
);



static const auto acceptButtonTitle =
/* i18-hint: Accept button it telemetry permission dialog */
XO ("Send anonymous analytics data");

static const auto rejectButtonTitle =
/* i18-hint: Reject button it telemetry permission dialog */
XO ("Don't send");


BEGIN_EVENT_TABLE (TelemetryDialog, wxDialogWrapper)
    EVT_BUTTON (wxID_YES, TelemetryDialog::OnAccept)
    EVT_BUTTON (wxID_NO, TelemetryDialog::OnDecline)
END_EVENT_TABLE ()

IMPLEMENT_CLASS (TelemetryDialog, wxDialogWrapper)

TelemetryDialog::TelemetryDialog (wxWindow* parent)
/* i18n-hint: information about the anonymous data collection */
    : wxDialogWrapper (parent, -1, XO ("Analytics & crash logging"),
        wxDefaultPosition, wxDefaultSize,
        wxCAPTION)
{
    ShuttleGui S (this, eIsCreating);

    wxFileSystem::AddHandler (new wxMemoryFSHandler);

    wxMemoryFSHandler::AddFile ("CheckMarkEmojii.png", bin2c_CheckMarkEmojii_png, sizeof (bin2c_CheckMarkEmojii_png));
    wxMemoryFSHandler::AddFile ("HeadphonesEmojii.png", bin2c_HeadphonesEmojii_png, sizeof (bin2c_HeadphonesEmojii_png));

    S.SetBorder (5);
    S.StartVerticalLay (wxEXPAND, 1);
    {
        S.AddWindow (CreateHTMLWindow (S.GetParent ()));

        S.StartHorizontalLay (wxEXPAND, 0);
        {
            S.Prop (1).AddSpace (1, 0, 1);

            S.Id (wxID_NO).AddButton (rejectButtonTitle);
            S.Id (wxID_YES).AddButton (acceptButtonTitle)->SetDefault ();

            S.AddSpace (5, 0);
        }
        S.EndHorizontalLay ();

        S.AddSpace (0, 5);
    }
    S.EndVerticalLay ();

    Layout ();
    Fit ();
    Center ();

    wxMemoryFSHandler::RemoveFile ("CheckMarkEmojii.png");
    wxMemoryFSHandler::RemoveFile ("HeadphonesEmojii.png");
}

TelemetryDialog::~TelemetryDialog ()
{

}

void TelemetryDialog::OnAccept (wxCommandEvent&)
{

    EndModal (wxID_YES);
}

void TelemetryDialog::OnDecline (wxCommandEvent&)
{
    EndModal (wxID_NO);
}

HtmlWindow* TelemetryDialog::CreateHTMLWindow (wxWindow* parent)
{
    wxStringOutputStream o;
    wxTextOutputStream informationStr (o);

    informationStr
        << wxT ("<html><body><h5>")
        << title 
        << "&nbsp&nbsp<img src=\"memory:CheckMarkEmojii.png\" width=\"18\" height=\"18\" />" //audacity::ToWXString (" 📈 🎧")
        << "&nbsp<img src=\"memory:HeadphonesEmojii.png\" width=\"18\" height=\"18\" />" //audacity::ToWXString (" 📈 🎧")
        << wxT ("</h5><p>")
        << firstParagraph
        << wxT ("</p><p><b>")
        << secondParagraph
        << wxT ("</b></p><p>")
        << thirdParagraph
        << wxT ("</p>")
        << wxT ("</body></html>");

    HtmlWindow* html = safenew LinkingHtmlWindow (parent, -1,
        wxDefaultPosition,
        wxSize (500, -1),
        wxHW_SCROLLBAR_NEVER | wxNO_BORDER | wxHW_NO_SELECTION);

    html->SetBorders (20); 
    //html->
    html->SetPage (o.GetString ());

    wxHtmlContainerCell* cell = html->GetInternalRepresentation ();

    cell->Layout (500);

    const wxSize size = wxSize (500, cell->GetHeight ());

    html->SetMinSize (size);
    html->SetMaxSize (size);
    html->SetSize (size);

    return html;
}
