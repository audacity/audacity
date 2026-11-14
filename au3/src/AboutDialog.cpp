/**********************************************************************

  Audacity: A Digital Audio Editor

  AboutDialog.cpp

  Dominic Mazzoni
  Vaughan Johnson
  James Crook

********************************************************************//**

\class AboutDialog
\brief The AboutDialog shows the program version and developer credits.

It is a simple scrolling window with an 'OK... Audacious!' button to
close it.

*//*****************************************************************//**

\class AboutDialogCreditItem
\brief AboutDialogCreditItem is a structure used by the AboutDialog to
hold information about one contributor to Audacity.

*//********************************************************************/

#include "AboutDialog.h"

#include <wx/html/htmlwin.h>
#include <wx/hyperlink.h>
#include <wx/statbmp.h>
#include <wx/sstream.h>
#include <wx/txtstrm.h>
#include <wx/statbox.h>
#include <wx/stattext.h>

#include "FileNames.h"
#include "HelpText.h"
#include "ShuttleGui.h"
#include "HelpSystem.h"
#include "AccessibleLinksFormatter.h"

#include "AllThemeResources.h"
#include "Theme.h"

#include "../images/AudacityLogoWithName.xpm"

// Notice this is a "system include".  This is on purpose and only until
// we convert over to CMake.  Once converted, the "RevisionIndent.h" file
// should be deleted and this can be changed back to a user include if
// desired.
//
// RevisionIdent.h may contain #defines like these ones:
//#define REV_LONG "28864acb238cb3ca71dda190a2d93242591dd80e"
//#define REV_TIME "Sun Apr 12 12:40:22 2015 +0100"
#include "RevisionIdent.h"

#ifndef REV_TIME
#define REV_TIME "unknown date and time"
#endif

#ifdef REV_LONG
#define REV_IDENT wxString("[[https://github.com/audacity/audacity/commit/") + REV_LONG + "|" + wxString(REV_LONG).Left(6) + "]] of " \
    + REV_TIME
#else
#define REV_IDENT (XO("No revision identifier was provided").Translation())
#endif

#if defined(HAS_SENTRY_REPORTING) || defined(HAVE_UPDATES_CHECK) || defined(USE_BREAKPAD)
#define HAS_PRIVACY_POLICY
#endif

// To substitute into many other translatable strings
static const auto ProgramName
    =//XO("Audacity");
      Verbatim("Audacity");

void AboutDialog::CreateCreditsList()
{
    const auto sysAdminFormat
        =/* i18n-hint: For "About Audacity..." credits, substituting a person's proper name */
          XO("%s, system administration");
    const auto coFounderFormat
        =/* i18n-hint: For "About Audacity..." credits, substituting a person's proper name */
          XO("%s, co-founder and developer");
    const auto designerFormat
        =/* i18n-hint: For "About Audacity..." credits, substituting a person's proper name */
          XO("%s, designer");
    const auto developerFormat
        =/* i18n-hint: For "About Audacity..." credits, substituting a person's proper name */
          XO("%s, developer");
    const auto developerAndSupprtFormat
        =/* i18n-hint: For "About Audacity..." credits, substituting a person's proper name */
          XO("%s, developer and support");
    const auto documentationAndSupportFormat
        =/* i18n-hint: For "About Audacity..." credits, substituting a person's proper name */
          XO("%s, documentation and support");
    const auto qaDocumentationAndSupportFormat
        =/* i18n-hint: For "About Audacity..." credits, substituting a person's proper name */
          XO("%s, QA tester, documentation and support");
    const auto documentationAndSupportFrenchFormat
        =/* i18n-hint: For "About Audacity..." credits, substituting a person's proper name */
          XO("%s, documentation and support, French");
    const auto qualityAssuranceFormat
        =/* i18n-hint: For "About Audacity..." credits, substituting a person's proper name */
          XO("%s, quality assurance");
    const auto accessibilityAdvisorFormat
        =/* i18n-hint: For "About Audacity..." credits, substituting a person's proper name */
          XO("%s, accessibility advisor");
    const auto graphicArtistFormat
        =/* i18n-hint: For "About Audacity..." credits, substituting a person's proper name */
          XO("%s, graphic artist");
    const auto composerFormat
        =/* i18n-hint: For "About Audacity..." credits, substituting a person's proper name */
          XO("%s, composer");
    const auto testerFormat
        =/* i18n-hint: For "About Audacity..." credits, substituting a person's proper name */
          XO("%s, tester");
    const auto NyquistPluginsFormat
        =/* i18n-hint: For "About Audacity..." credits, substituting a person's proper name */
          XO("%s, Nyquist plug-ins");
    const auto webDeveloperFormat
        =/* i18n-hint: For "About Audacity..." credits, substituting a person's proper name */
          XO("%s, web developer");
    const auto graphicsFormat
        =/* i18n-hint: For "About Audacity..." credits, substituting a person's proper name */
          XO("%s, graphics");
    const auto presetsFormat
        =/* i18n-hint: For "About Audacity..." credits, substituting a person's proper name */
          XO("%s, effects presets");

    // The Audacity Team: developers and support
    AddCredit(wxT("Antons \u010cinakovs"), testerFormat, roleTeamMember);
    AddCredit(wxT("Matthieu Hodgkinson"), developerFormat, roleTeamMember);
    AddCredit(wxT("Peter Jonas"), developerFormat, roleTeamMember);
    AddCredit(wxT("Martin Keary"), roleTeamMember);
    AddCredit(wxT("Sergey Lapysh"), testerFormat, roleTeamMember);
    AddCredit(wxT("Yana Larina"), roleTeamMember);
    AddCredit(wxT("Dilson's Pickles"), designerFormat, roleTeamMember);
    AddCredit(wxT("Anita Sudan"), roleTeamMember);
    AddCredit(wxT("Vitaly Sverchinsky"), developerFormat, roleTeamMember);
    AddCredit(wxT("Leo Wattenberg"), designerFormat, roleTeamMember);
    AddCredit(wxT("Jessica Williamson"), designerFormat, roleTeamMember);

    // Emeritus: people who were "lead developers" or made an
    // otherwise distinguished contribution, but who are no
    // longer active.
    AddCredit(wxT("Gale Andrews"), qualityAssuranceFormat, roleEmeritusTeam);
    AddCredit(wxT("Richard Ash"), developerFormat, roleEmeritusTeam);
    AddCredit(wxT("Christian Brochec"),
              documentationAndSupportFrenchFormat, roleEmeritusTeam);
    AddCredit(wxT("Matt Brubeck"), developerFormat, roleEmeritusTeam);
    AddCredit(wxT("Arturo \"Buanzo\" Busleiman"), sysAdminFormat, roleEmeritusTeam);
    AddCredit(wxT("Michael Chinen"), developerFormat, roleEmeritusTeam);
    AddCredit(wxT("James Crook"), developerFormat, roleEmeritusTeam);
    AddCredit(wxT("Roger Dannenberg"), coFounderFormat, roleEmeritusTeam);
    AddCredit(wxT("Steve Daulton"), roleEmeritusTeam);
    AddCredit(wxT("Al Dimond"), developerFormat, roleEmeritusTeam);
    AddCredit(wxT("Benjamin Drung"), developerFormat, roleEmeritusTeam);
    AddCredit(wxT("Joshua Haberman"), developerFormat, roleEmeritusTeam);
    AddCredit(wxT("Ruslan Ijbulatov"), developerFormat, roleEmeritusTeam);
    AddCredit(wxT("Vaughan Johnson"), developerFormat, roleEmeritusTeam);
    AddCredit(wxT("Greg Kozikowski"), documentationAndSupportFormat, roleEmeritusTeam);
    AddCredit(wxT("Paul Licameli"), developerFormat, roleEmeritusTeam);
    AddCredit(wxT("Leland Lucius"), developerFormat, roleEmeritusTeam);
    AddCredit(wxT("Dominic Mazzoni"), coFounderFormat, roleEmeritusTeam);
    AddCredit(wxT("Markus Meyer"), developerFormat, roleEmeritusTeam);
    AddCredit(wxT("Monty Montgomery"), developerFormat, roleEmeritusTeam);
    AddCredit(wxT("Shane Mueller"), developerFormat, roleEmeritusTeam);
    AddCredit(wxT("Tony Oetzmann"), documentationAndSupportFormat, roleEmeritusTeam);
    AddCredit(wxT("Alexandre Prokoudine"), documentationAndSupportFormat, roleEmeritusTeam);
    AddCredit(wxT("Peter Sampson"), qaDocumentationAndSupportFormat, roleEmeritusTeam);
    AddCredit(wxT("Martyn Shaw"), developerFormat, roleEmeritusTeam);
    AddCredit(wxT("Dmitry Vedenko"), developerFormat, roleEmeritusTeam);
    AddCredit(wxT("Bill Wharrie"), documentationAndSupportFormat, roleEmeritusTeam);

    // Contributors
    AddCredit(wxT("Lynn Allan"), developerFormat, roleContributor);
    AddCredit(wxT("Johan Althoff (teetow)"), designerFormat, roleContributor);
    AddCredit(wxT("Brian Armstrong"), developerFormat, roleContributor);
    AddCredit(wxT("David Avery"), developerFormat, roleContributor);
    AddCredit(wxT("David Bailes"), accessibilityAdvisorFormat, roleContributor);
    AddCredit(wxT("Brian Beard (Kurtsley)"), developerFormat, roleContributor);
    AddCredit(wxT("William Bland"), developerFormat, roleContributor);
    AddCredit(wxT("Sami Boukortt"), developerFormat, roleContributor);
    AddCredit(wxT("Jeremy R. Brown"), developerFormat, roleContributor);
    AddCredit(wxT("Alex S. Brown"), developerFormat, roleContributor);
    AddCredit(wxT("David Bryant"), developerFormat, roleContributor);
    AddCredit(wxT("Chris Cannam"), developerFormat, roleContributor);
    AddCredit(wxT("Subhradeep Chakraborty"), developerFormat, roleContributor);
    AddCredit(wxT("Cory Cook"), developerFormat, roleContributor);
    AddCredit(wxT("Craig DeForest"), developerFormat, roleContributor);
    AddCredit(wxT("Edgar Franke (Edgar-RFT)"), developerFormat, roleContributor);
    AddCredit(wxT("Anton Gerasimov"), developerFormat, roleContributor);
    AddCredit(wxT("Mitch Golden"), developerFormat, roleContributor);
    AddCredit(wxT("Brian Gunlogson"), developerFormat, roleContributor);
    AddCredit(wxT("Gonzalo Guzm\u00E1n"), documentationAndSupportFormat, roleContributor);
    AddCredit(wxT("Andrew Hallendorff"), developerFormat, roleContributor);
    AddCredit(wxT("Robert H\u00E4nggi"), developerFormat, roleContributor);
    AddCredit(wxT("Jouni Helminen"), designerFormat, roleContributor);
    AddCredit(wxT("Daniel Horgan"), developerFormat, roleContributor);
    AddCredit(wxT("David Hostetler"), developerFormat, roleContributor);
    AddCredit(wxT("Edward Hui"), developerFormat, roleContributor);
    AddCredit(wxT("Vladislav Isaev"), presetsFormat, roleContributor);
    AddCredit(wxT("Marek Iwaszkiewicz"), presetsFormat, roleContributor);
    AddCredit(wxT("Steve Jolly"), developerFormat, roleContributor);
    AddCredit(wxT("Steven Jones"), developerFormat, roleContributor);
    AddCredit(wxT("Henric Jungheim"), developerFormat, roleContributor);
    AddCredit(wxT("Myungchul Keum"), developerFormat, roleContributor);
    AddCredit(wxT("Arun Kishore"), developerFormat, roleContributor);
    AddCredit(wxT("Paul Livesey"), developerFormat, roleContributor);
    AddCredit(wxT("Harvey Lubin"), graphicArtistFormat, roleContributor);
    AddCredit(wxT("Max Maisel"), developerFormat, roleContributor);
    AddCredit(wxT("Pietro Marcello"), developerFormat, roleContributor);
    AddCredit(wxT("Greg Mekkes"), developerFormat, roleContributor);
    AddCredit(wxT("Abe Milde"), developerFormat, roleContributor);
    AddCredit(wxT("Ryan Miller"), testerFormat, roleContributor);
    AddCredit(wxT("Paul Nasca"), developerFormat, roleContributor);
    AddCredit(wxT("Clayton Otey"), developerFormat, roleContributor);
    AddCredit(wxT("Pavel Penikov"), testerFormat, roleContributor);
    AddCredit(wxT("Mark Phillips"), developerFormat, roleContributor);
    AddCredit(wxT("Andr\u00E9 Pinto"), developerFormat, roleContributor);
    AddCredit(wxT("Pokechu22"), developerFormat, roleContributor);
    AddCredit(wxT("Jean Claude Risset"), composerFormat, roleContributor);
    AddCredit(wxT("RuRo"), developerFormat, roleContributor);
    AddCredit(wxT("Augustus Saunders"), developerFormat, roleContributor);
    AddCredit(wxT("Benjamin Schwartz"), developerFormat, roleContributor);
    AddCredit(wxT("Cliff Scott"), testerFormat, roleContributor);
    AddCredit(wxT("David R. Sky"), NyquistPluginsFormat, roleContributor);
    AddCredit(wxT("Joe Souza"), developerFormat, roleContributor);
    AddCredit(wxT("K. Soze"), developerFormat, roleContributor);
    AddCredit(wxT("Rob Sykes"), developerFormat, roleContributor);
    AddCredit(wxT("Mike Underwood"), developerFormat, roleContributor);
    AddCredit(wxT("Philip Van Baren"), developerFormat, roleContributor);
    AddCredit(wxT("Salvo Ventura"), developerFormat, roleContributor);
    AddCredit(wxT("Darrell Walisser"), developerFormat, roleContributor);
    AddCredit(wxT("Jun Wan"), developerFormat, roleContributor);
    AddCredit(wxT("Daniel Winzen"), developerFormat, roleContributor);
    AddCredit(wxT("Tom Woodhams"), developerFormat, roleContributor);
    AddCredit(wxT("Mark Young"), developerFormat, roleContributor);
    AddCredit(wxT("Wing Yu"), developerFormat, roleContributor);

    // Website and Graphics
    AddCredit(wxT("Shinta Carolinasari"), webDeveloperFormat, roleGraphics);
    AddCredit(wxT("Bayu Rizaldhan Rayes"), graphicsFormat, roleGraphics);

    // Libraries

    AddCredit(wxT("[[https://libexpat.github.io/|expat]]"), roleLibrary);
    AddCredit(wxT("[[https://xiph.org/flac/|FLAC]]"), roleLibrary);
    AddCredit(wxT("[[http://lame.sourceforge.net/|LAME]]"), roleLibrary);
    AddCredit(wxT("[[http://www.mega-nerd.com/libsndfile/|libsndfile]]"), roleLibrary);
    AddCredit(wxT("[[https://sourceforge.net/p/soxr/wiki/Home/|libsoxr]]"), roleLibrary);
    AddCredit(
        XO("%s (incorporating %s, %s, %s, %s and %s)")
        .Format(
            "[[http://lv2plug.in/|lv2]]",
            "lilv",
            "msinttypes",
            "serd",
            "sord",
            "sratom"
            ).Translation(),
        roleLibrary);
    AddCredit(wxT("[[https://www.cs.cmu.edu/~music/nyquist/|Nyquist]]"), roleLibrary);
    AddCredit(wxT("[[https://xiph.org/vorbis/|Ogg Vorbis]]"), roleLibrary);
    AddCredit(wxT("[[http://www.portaudio.com/|PortAudio]]"), roleLibrary);
    AddCredit(wxT("[[http://www.portmedia.sourceforge.net/portmidi/|PortMidi]]"), roleLibrary);
    AddCredit(wxT("[[https://sourceforge.net/p/portmedia/wiki/portsmf/|portsmf]]"), roleLibrary);
    AddCredit(wxT("[[http://sbsms.sourceforge.net/|sbsms]]"), roleLibrary);
    AddCredit(wxT("[[https://www.surina.net/soundtouch/|SoundTouch]]"), roleLibrary);
    AddCredit(wxT("[[http://www.twolame.org/|TwoLAME]]"), roleLibrary);
    AddCredit(wxT("[[http://www.vamp-plugins.org/|Vamp]]"), roleLibrary);
    AddCredit(wxT("[[https://wxwidgets.org/|wxWidgets]]"), roleLibrary);

    // Thanks

    AddCredit(wxT("Dave Beydler"), roleThanks);
    AddCredit(wxT("Brian Cameron"), roleThanks);
    AddCredit(wxT("Jason Cohen"), roleThanks);
    AddCredit(wxT("Dave Fancella"), roleThanks);
    AddCredit(wxT("Steve Harris"), roleThanks);
    AddCredit(wxT("Daniel James"), roleThanks);
    AddCredit(wxT("Daniil Kolpakov"), roleThanks);
    AddCredit(wxT("Robert Leidle"), roleThanks);
    AddCredit(wxT("Logan Lewis"), roleThanks);
    AddCredit(wxT("David Luff"), roleThanks);
    AddCredit(wxT("Jason Pepas"), roleThanks);
    AddCredit(wxT("Jonathan Ryshpan"), roleThanks);
    AddCredit(wxT("Michael Schwendt"), roleThanks);
    AddCredit(wxT("Patrick Shirkey"), roleThanks);
    AddCredit(wxT("Tuomas Suutari"), roleThanks);
    AddCredit(wxT("Mark Tomlinson"), roleThanks);
    AddCredit(wxT("David Topper"), roleThanks);
    AddCredit(wxT("Rudy Trubitt"), roleThanks);
    AddCredit(wxT("StreetIQ.com"), roleThanks);
    AddCredit(wxT("UmixIt Technologies, LLC"), roleThanks);
    AddCredit(wxT("Verilogix, Inc."), roleThanks);
}

// ----------------------------------------------------------------------------

BEGIN_EVENT_TABLE(AboutDialog, wxDialogWrapper)
EVT_BUTTON(wxID_OK, AboutDialog::OnOK)
END_EVENT_TABLE()

IMPLEMENT_CLASS(AboutDialog, wxDialogWrapper)

namespace {
AboutDialog* sActiveInstance{};
}

AboutDialog* AboutDialog::ActiveIntance()
{
    return sActiveInstance;
}

AboutDialog::AboutDialog(wxWindow* parent)
/* i18n-hint: information about the program */
    :  wxDialogWrapper(parent, -1, XO("About %s").Format(ProgramName),
                       wxDefaultPosition, wxDefaultSize,
                       wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER)
{
    wxASSERT(!sActiveInstance);
    sActiveInstance = this;

    SetName();
    this->SetBackgroundColour(theTheme.Colour(clrAboutBoxBackground));
    //this->SetBackgroundColour(theTheme.Colour( clrMedium ));
    icon = NULL;
    ShuttleGui S(this, eIsCreating);
    S.StartNotebook();
    {
        PopulateAudacityPage(S);
        PopulateInformationPage(S);
        PopulateLicensePage(S);
    }
    S.EndNotebook();

    S.Id(wxID_OK)
    .Prop(0)
    .AddButton(XXO("OK"), wxALIGN_CENTER, true);

    Layout();
    Fit();
    this->Centre();
}

#define ABOUT_DIALOG_WIDTH 506

void AboutDialog::PopulateAudacityPage(ShuttleGui& S)
{
    CreateCreditsList();

    /* i18n-hint: The translation of "translator_credits" will appear
     *  in the credits in the About Audacity window.  Use this to add
     *  your own name(s) to the credits.
     *
     *  For example:  "English translation by Dominic Mazzoni." */
    auto translatorCreditsMsgid = XO("translator_credits");
    auto translatorCredits = translatorCreditsMsgid.Translation();
    if (translatorCredits == translatorCreditsMsgid.MSGID().GET()) {
        // We're in an English locale
        translatorCredits.clear();
    } else {
        translatorCredits += wxT("<br>");
    }

    wxStringOutputStream o;
    wxTextOutputStream informationStr(o);    // string to build up list of information in
    informationStr
        << wxT("<center>")
        << XO("<h3>")
        << ProgramName
        << wxT(" ")
        << wxString(AUDACITY_VERSION_STRING)
        << wxT("</center></h3>")
        /* i18n-hint: The program's name substitutes for %s */
        << XO("%s the free, open source, cross-platform software for recording and editing sounds.")
        .Format(ProgramName)

        << wxT("<h3>")
        << XO("Credits")
        << wxT("</h3>")

        << wxT("<p><b>")
        /* i18n-hint: The program's name substitutes for %s */
        << XO("%s Team Members").Format(ProgramName)
        << wxT("</b><br>")
        << GetCreditsByRole(roleTeamMember)

        << wxT("<p><b> ")
        << XO("Emeritus:")
        << wxT("</b><br>")
        /* i18n-hint: The program's name substitutes for %s */
        << XO("Distinguished %s Team members, not currently active")
        .Format(ProgramName)
        << wxT("<br><br>")
        << GetCreditsByRole(roleEmeritusTeam)

        << wxT("<p><b>")
        << XO("Contributors")
        << wxT("</b><br>")
        << GetCreditsByRole(roleContributor)

        << wxT("<p><b>")
        << XO("Website and Graphics")
        << wxT("</b><br>")
        << GetCreditsByRole(roleGraphics)
    ;

    if (!translatorCredits.empty()) {
        informationStr
            << wxT("<p><b>")
            << XO("Translators")
            << wxT("</b><br>")
            << translatorCredits
        ;
    }

    informationStr
        << wxT("<p><b>")
        << XO("Libraries")
        << wxT("</b><br>")
        /* i18n-hint: The program's name substitutes for %s */
        << XO("%s includes code from the following projects:").Format(ProgramName)
        << wxT("<br><br>")
        << GetCreditsByRole(roleLibrary)

        << wxT("<p><b>")
        << XO("Special thanks:")
        << wxT("</b><br>")
        << GetCreditsByRole(roleThanks)

        << wxT("<p><br>")
        /* i18n-hint: The program's name substitutes for %s */
        << XO("%s website: ").Format(ProgramName)
        << wxT("[[https://www.audacityteam.org/|https://www.audacityteam.org/]]")

        << wxT("<p><br>&nbsp; &nbsp; ")
        /* i18n-hint Audacity's name substitutes for first and third %s,
         and a "copyright" symbol for the second */
        << XO("%s software is copyright %s 1999-2024 %s Team.")
        .Format(
        Verbatim("<b>%s<sup>&reg;</sup></b>").Format(ProgramName),
        wxT("&copy;"),
        ProgramName)
        << wxT("<br>")

        << wxT("&nbsp; &nbsp; ")
        /* i18n-hint Audacity's name substitutes for %s */
        << XO("The name %s is a registered trademark.")
        .Format(Verbatim("<b>%s</b>").Format(ProgramName))
        << wxT("<br><br>")
        << wxT("</center>")
    ;

    auto pPage = S.StartNotebookPage(ProgramName);
    S.StartVerticalLay(1);
    {
        //v For now, change to AudacityLogoWithName via old-fashioned way, not Theme.
        wxBitmap logo(AudacityLogoWithName_xpm); //v

        //Setup to scale the logo larger and smaller as necessary
        const float fScale = 1.0f;
        wxImage RescaledImage(logo.ConvertToImage());
        wxColour MainColour(
            RescaledImage.GetRed(1, 1),
            RescaledImage.GetGreen(1, 1),
            RescaledImage.GetBlue(1, 1));
        pPage->SetBackgroundColour(MainColour);
        // wxIMAGE_QUALITY_HIGH not supported by wxWidgets 2.6.1, or we would use it here.
        RescaledImage.Rescale((int)(LOGOWITHNAME_WIDTH * fScale), (int)(LOGOWITHNAME_HEIGHT * fScale));
        wxBitmap RescaledBitmap(RescaledImage);

        icon
            =safenew wxStaticBitmap(S.GetParent(), -1,
                                    //*logo, //v
                                    //v theTheme.Bitmap(bmpAudacityLogo), wxPoint(93, 10), wxSize(215, 190));
                                    //v theTheme.Bitmap(bmpAudacityLogoWithName),
                                    RescaledBitmap,
                                    wxDefaultPosition,
                                    wxSize((int)(LOGOWITHNAME_WIDTH * fScale), (int)(LOGOWITHNAME_HEIGHT * fScale)));
    }
    S.Prop(0).AddWindow(icon);

    HtmlWindow* html = safenew LinkingHtmlWindow(S.GetParent(), -1,
                                                 wxDefaultPosition,
                                                 wxSize(ABOUT_DIALOG_WIDTH, 359),
                                                 wxHW_SCROLLBAR_AUTO);
    html->SetPage(FormatHtmlText(o.GetString()));

    /* locate the html renderer where it fits in the dialogue */
    S.Prop(1).Position(wxEXPAND).Focus()
    .AddWindow(html);

    S.EndVerticalLay();
    S.EndNotebookPage();
}

/** \brief: Fills out the "Information" tab of the preferences dialogue
 *
 * Provides as much information as possible about build-time options and
 * the libraries used, to try and make Linux support easier. Basically anything
 * about the build we might wish to know should be visible here */
void AboutDialog::PopulateInformationPage(ShuttleGui& S)
{
    wxStringOutputStream o;
    wxTextOutputStream informationStr(o);    // string to build up list of information in
    S.StartNotebookPage(XO("Build Information"));   // start the tab
    S.StartVerticalLay(2); // create the window
    HtmlWindow* html = safenew LinkingHtmlWindow(S.GetParent(), -1, wxDefaultPosition,
                                                 wxSize(ABOUT_DIALOG_WIDTH, 264),
                                                 wxHW_SCROLLBAR_AUTO);
    // create a html pane in it to put the content in.
    auto enabled = XO("Enabled");
    auto disabled = XO("Disabled");
    wxString blank;

    /* this builds up the list of information to go in the window in the string
     * informationStr */

    informationStr
        << wxT("<h3>")
        /* i18n-hint: Information about when audacity was compiled follows */
        << XO("The Build")
        << wxT("</h3>\n<table>"); // start build info table

    // Current date
    AddBuildinfoRow(&informationStr, XO("Program build date:"), __TDATE__);
    AddBuildinfoRow(&informationStr, XO("Commit Id:"), REV_IDENT);

    auto buildType =
#ifdef _DEBUG
        XO("Debug build (debug level %d)").Format(wxDEBUG_LEVEL);
#else
        XO("Release build (debug level %d)").Format(wxDEBUG_LEVEL);
#endif
    ;
    if ((sizeof(void*) == 8)) {
        buildType = XO("%s, 64 bits").Format(buildType);
    } else {
        buildType = XO("%s, 32 bits").Format(buildType);
    }
// Remove this once the transition to CMake is complete
#if defined(CMAKE)
    buildType = Verbatim("CMake %s").Format(buildType);
#endif

    AddBuildinfoRow(&informationStr, XO("Build type:"), buildType.Translation());

#ifdef _MSC_FULL_VER
    AddBuildinfoRow(&informationStr, XO("Compiler:"),
                    wxString::Format(wxT("MSVC %02d.%02d.%05d.%02d"), _MSC_VER / 100, _MSC_VER % 100, _MSC_FULL_VER % 100000, _MSC_BUILD));
#endif

#ifdef __GNUC_PATCHLEVEL__
#ifdef __MINGW32__
    AddBuildinfoRow(&informationStr, XO("Compiler:"),
                    wxT("MinGW ") wxMAKE_VERSION_DOT_STRING_T(__GNUC__, __GNUC_MINOR__, __GNUC_PATCHLEVEL__));
#else
    AddBuildinfoRow(&informationStr, XO("Compiler:"),
                    wxT("GCC ") wxMAKE_VERSION_DOT_STRING_T(__GNUC__, __GNUC_MINOR__, __GNUC_PATCHLEVEL__));
#endif
#endif

#ifdef __clang_version__
    AddBuildinfoRow(&informationStr, XO("Compiler:"), wxT("clang ") __clang_version__);
#endif

    // Install prefix
#ifdef __WXGTK__
    /* i18n-hint: The directory audacity is installed into (on *nix systems) */
    AddBuildinfoRow(&informationStr, XO("Installation Prefix:"), \
                    wxT(INSTALL_PREFIX));
#endif

    // Location of cache
    AddBuildinfoRow(&informationStr, XO("Cache folder:"), \
                    FileNames::CacheDir());
    // Location of settings
    AddBuildinfoRow(&informationStr, XO("Settings folder:"), \
                    FileNames::ConfigDir());
    // Location of data
    AddBuildinfoRow(&informationStr, XO("Data folder:"), \
                    FileNames::DataDir());
    // Location of data
    AddBuildinfoRow(&informationStr, XO("State folder:"), \
                    FileNames::StateDir());

    informationStr << wxT("</table>\n"); // end of build info table

    informationStr
        << wxT("<h3>")
        /* i18n-hint: Libraries that are essential to audacity */
        << XO("Core Libraries")
        << wxT("</h3>\n<table>"); // start table of core libraries

    AddBuildinfoRow(&informationStr, wxT("wxWidgets"),
                    XO("Cross-platform GUI library"), Verbatim(wxVERSION_NUM_DOT_STRING_T));

    AddBuildinfoRow(&informationStr, wxT("PortAudio"),
                    XO("Audio playback and recording"), Verbatim(wxT("v19")));

    AddBuildinfoRow(&informationStr, wxT("libsoxr"),
                    XO("Sample rate conversion"), enabled);

    informationStr << wxT("</table>\n"); // end table of core libraries

    informationStr
        << wxT("<h3>")
        << XO("File Format Support")
        << wxT("</h3>\n<p>");

    informationStr
        << wxT("<table>"); // start table of file formats supported

   #if defined(USE_LIBMPG123)
    AddBuildinfoRow(&informationStr, wxT("libmpg123"), XO("MP3 Import"), enabled);
   #else
    AddBuildinfoRow(&informationStr, wxT("libmpg123"), XO("MP3 Import"), disabled);
   #endif

   #if USE_LIBMP3LAME
    AddBuildinfoRow(
        &informationStr, wxT("libmp3lame"),
        /* i18n-hint: LAME is the codec name. This name should not be translated
         */
        XO("MP3 Export"), enabled);
   #else
    AddBuildinfoRow(
        &informationStr, wxT("libopus"),
        /* i18n-hint: Opus is the codec name. This name should not be translated
         */
        XO("Opus Import and Export"), disabled);
   #endif

   #ifdef USE_LIBVORBIS
    AddBuildinfoRow(&informationStr, wxT("libvorbis"),
                    /* i18n-hint: Ogg is the container format. Vorbis is the compression codec.
                     * Both are proper nouns and shouldn't be translated */
                    XO("Ogg Vorbis Import and Export"), enabled);
   #else
    AddBuildinfoRow(&informationStr, wxT("libvorbis"),
                    XO("Ogg Vorbis Import and Export"), disabled);
   #endif

   #if USE_LIBVORBIS && USE_LIBOPUS && USE_OPUSFILE
    AddBuildinfoRow(
        &informationStr, wxT("libopus"),
        /* i18n-hint: Opus is the codec name. This name should not be translated */
        XO("Opus Import and Export"), enabled);
   #else
    AddBuildinfoRow(
        &informationStr, wxT("libopus"),
        /* i18n-hint: Opus is the codec name. This name should not be translated
         */
        XO("Opus Import and Export"), disabled);
   #endif

   #ifdef USE_LIBID3TAG
    AddBuildinfoRow(&informationStr, wxT("libid3tag"), XO("ID3 tag support"),
                    enabled);
   #else
    AddBuildinfoRow(&informationStr, wxT("libid3tag"), XO("ID3 tag support"),
                    disabled);
   #endif

   # if USE_LIBFLAC
    /* i18n-hint: FLAC stands for Free Lossless Audio Codec, but is effectively
     * a proper noun and so shouldn't be translated */
    AddBuildinfoRow(&informationStr, wxT("libflac"), XO("FLAC import and export"),
                    enabled);
   # else
    AddBuildinfoRow(&informationStr, wxT("libflac"), XO("FLAC import and export"),
                    disabled);
   # endif

   # if USE_LIBTWOLAME
    AddBuildinfoRow(&informationStr, wxT("libtwolame"), XO("MP2 export"),
                    enabled);
   # else
    AddBuildinfoRow(&informationStr, wxT("libtwolame"), XO("MP2 export"),
                    disabled);
   # endif

   #ifdef USE_FFMPEG
    AddBuildinfoRow(&informationStr, wxT("ffmpeg"), XO("FFmpeg Import/Export"), enabled);
   #else
    AddBuildinfoRow(&informationStr, wxT("ffmpeg"), XO("FFmpeg Import/Export"), disabled);
   #endif

    informationStr << wxT("</table>\n"); //end table of file formats supported

    informationStr
        << wxT("<h3>")
        << XO("Features")
        << wxT("</h3>\n<table>"); // start table of features

   # if USE_NYQUIST
    AddBuildinfoRow(&informationStr, wxT("Nyquist"), XO("Plug-in support"),
                    enabled);
   # else
    AddBuildinfoRow(&informationStr, wxT("Nyquist"), XO("Plug-in support"),
                    disabled);
   # endif

   # if USE_LADSPA
    AddBuildinfoRow(&informationStr, wxT("LADSPA"), XO("Plug-in support"),
                    enabled);
   # else
    AddBuildinfoRow(&informationStr, wxT("LADSPA"), XO("Plug-in support"),
                    disabled);
   # endif

   # if USE_VAMP
    AddBuildinfoRow(&informationStr, wxT("Vamp"), XO("Plug-in support"),
                    enabled);
   # else
    AddBuildinfoRow(&informationStr, wxT("Vamp"), XO("Plug-in support"),
                    disabled);
   # endif

   # if USE_AUDIO_UNITS
    AddBuildinfoRow(&informationStr, wxT("Audio Units"), XO("Plug-in support"),
                    enabled);
   # else
    AddBuildinfoRow(&informationStr, wxT("Audio Units"), XO("Plug-in support"),
                    disabled);
   # endif

   # if USE_VST
    AddBuildinfoRow(&informationStr, wxT("VST"), XO("Plug-in support"),
                    enabled);
   # else
    AddBuildinfoRow(&informationStr, wxT("VST"), XO("Plug-in support"),
                    disabled);
   # endif

   # if USE_LV2
    AddBuildinfoRow(&informationStr, wxT("LV2"), XO("Plug-in support"),
                    enabled);
   # else
    AddBuildinfoRow(&informationStr, wxT("LV2"), XO("Plug-in support"),
                    disabled);
   # endif

   # if USE_PORTMIXER
    AddBuildinfoRow(&informationStr, wxT("PortMixer"), XO("Sound card mixer support"),
                    enabled);
   # else
    AddBuildinfoRow(&informationStr, wxT("PortMixer"), XO("Sound card mixer support"),
                    disabled);
   # endif

   # if USE_SOUNDTOUCH
    AddBuildinfoRow(&informationStr, wxT("SoundTouch"), XO("Pitch and Tempo Change support"),
                    enabled);
   # else
    AddBuildinfoRow(&informationStr, wxT("SoundTouch"), XO("Pitch and Tempo Change support"),
                    disabled);
   # endif

   # if USE_SBSMS
    AddBuildinfoRow(&informationStr, wxT("SBSMS"), XO("Extreme Pitch and Tempo Change support"),
                    enabled);
   # else
    AddBuildinfoRow(&informationStr, wxT("SBSMS"), XO("Extreme Pitch and Tempo Change support"),
                    disabled);
   # endif

    informationStr << wxT("</table>\n");  // end of table of features

    html->SetPage(FormatHtmlText(o.GetString()));      // push the page into the html renderer
    S.Prop(2)
    .Position(wxEXPAND)
    .AddWindow(html);     // make it fill the page
    // I think the 2 here goes with the StartVerticalLay() call above?
    S.EndVerticalLay();    // end window
    S.EndNotebookPage(); // end the tab
}

const wxString GPL_TEXT();

void AboutDialog::PopulateLicensePage(ShuttleGui& S)
{
#if defined(HAS_PRIVACY_POLICY)
    S.StartNotebookPage(XC("Legal", "about dialog"));
#else
    S.StartNotebookPage(XO("GPL License"));
#endif

#if defined(HAS_PRIVACY_POLICY)
    S.Prop(0).StartPanel();
    {
        S.AddSpace(0, 8);
        /* i18n-hint: For "About Audacity...": Title for Privacy Policy section */
        S.AddVariableText(XC("PRIVACY POLICY", "about dialog"), true);

        S.AddFixedText(
            XO("App update checking and error reporting require network access. "
               "These features are optional."));

        /* i18n-hint: %s will be replaced with "our Privacy Policy" */
        AccessibleLinksFormatter privacyPolicy(XO("See %s for more info."));

        privacyPolicy.FormatLink(
            /* i18n-hint: Title of hyperlink to the privacy policy. This is an object of "See". */
            wxT("%s"), XO("our Privacy Policy"),
            "https://www.audacityteam.org/about/desktop-privacy-notice/");

        privacyPolicy.Populate(S);
    }
    S.EndPanel();

    S.AddSpace(0, 8);
#endif

    S.Prop(1).StartPanel();
    {
        HtmlWindow* html = safenew LinkingHtmlWindow(
            S.GetParent(), -1, wxDefaultPosition, wxSize(ABOUT_DIALOG_WIDTH, 264),
            wxHW_SCROLLBAR_AUTO);

        html->SetPage(FormatHtmlText(GPL_TEXT()));

        S.Prop(1).Position(wxEXPAND).AddWindow(html);
    }
    S.EndPanel();

    S.EndNotebookPage();
}

void AboutDialog::AddCredit(const wxString& name, Role role)
{
    AddCredit(name, {}, role);
}

void AboutDialog::AddCredit(
    const wxString& name, TranslatableString format, Role role)
{
    auto str = format.empty()
               ? Verbatim(name)
               : TranslatableString{ format }.Format(name);
    creditItems.emplace_back(std::move(str), role);
}

wxString AboutDialog::GetCreditsByRole(AboutDialog::Role role)
{
    wxString s;

    for (const auto& item : creditItems) {
        if (item.role == role) {
            s += item.description.Translation();
            s += wxT("<br>");
        }
    }

    // Strip last <br>, if any
    if (s.Right(4) == wxT("<br>")) {
        s = s.Left(s.length() - 4);
    }

    return s;
}

/** \brief Add a table row saying if a library is used or not
 *
 * Used when creating the build information tab to show if each optional
 * library is enabled or not, and what it does */
void AboutDialog::AddBuildinfoRow(
    wxTextOutputStream* str, const wxChar* libname,
    const TranslatableString& libdesc, const TranslatableString& status)
{
    *str
        << wxT("<tr><td>")
        << libname
        << wxT("</td><td>(")
        << libdesc
        << wxT(")</td><td>")
        << status
        << wxT("</td></tr>");
}

/** \brief Add a table row saying if a library is used or not
 *
 * Used when creating the build information tab to show build dates and
 * file paths */
void AboutDialog::AddBuildinfoRow(
    wxTextOutputStream* str,
    const TranslatableString& description, const wxChar* spec)
{
    *str
        << wxT("<tr><td>")
        << description
        << wxT("</td><td>")
        << spec
        << wxT("</td></tr>");
}

AboutDialog::~AboutDialog()
{
    sActiveInstance = {};
}

void AboutDialog::OnOK(wxCommandEvent& WXUNUSED(event))
{
#ifdef __WXMAC__
    Destroy();
#else
    EndModal(wxID_OK);
#endif
}
