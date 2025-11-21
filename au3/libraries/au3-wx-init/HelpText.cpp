/**********************************************************************

  Audacity: A Digital Audio Editor

  HelpText.cpp

  James Crook

********************************************************************//**

\file HelpText.cpp
\brief Given a key, returns some html.
*//********************************************************************/

#include "HelpText.h"

#include <wx/colour.h>
#include <wx/sstream.h>
#include <wx/txtstrm.h>

#include "FileNames.h"
#include "Internat.h"
#include "AllThemeResources.h"
#include "Theme.h"

wxString HtmlColourOfIndex(int i)
{
    wxColour c =  theTheme.Colour(i);
    return wxString::Format("\"#%02X%02X%02X\"",
                            c.Red(), c.Green(), c.Blue());
}

static wxString WrapText(const wxString& Text)
{
    return wxString(wxT(""))
           + wxT("<html><head></head>")
           + wxT("<body bgcolor=") + HtmlColourOfIndex(clrTrackInfo) + wxT(">")
           + wxT("<font color=") + HtmlColourOfIndex(clrTrackPanelText) + wxT(">")
           + wxT("<p>") + Text
           + wxT("</font>")
           + wxT("</body></html>");
}

static wxString InnerLink(const wxString& Key, const wxString& Text)
{
    return wxString(wxT(""))
           + wxT("<a href='innerlink:")
           + Key
           + wxT("'>")
           + Text
           + wxT("</a>");
}

static wxString FileLink(const wxString& Key, const wxString& Text)
{
    return wxString(wxT(""))
           + wxT("<a href='")
           + wxT("file:")
           + FileNames::HtmlHelpDir()
           + Key
           + wxT("'>")
           + Text
           + wxT("</a>");
}

static wxString TypedLink(const wxString& Key, const wxString& Text)
{
    return wxString(wxT(""))
           + wxT("<a href='")
           + Key
           + wxT("'>")
           + Text
           + wxT("</a>");
}

static wxString LinkExpand(const wxString& Text)
{
    wxString Temp = Text;
    int i, j, k;
    while ((i=Temp.First(wxT("[["))) != wxNOT_FOUND)
    {
        wxString Key = Temp.Mid(i + 2);
        j = Key.First(wxT("|"));
        if (j == wxNOT_FOUND) {
            return Temp;
        }
        wxString LinkText = Key.Mid(j + 1);
        k = LinkText.First(wxT("]]"));
        if (k == wxNOT_FOUND) {
            return Temp;
        }
        Key = Key.Mid(0, j);
        LinkText = LinkText.Mid(0, k);

        LinkText=wxString("<font color=") + HtmlColourOfIndex(clrSample) + wxT(">") + LinkText + "</font>";
        wxString Replacement;
        if (Key.StartsWith(wxT("file:"))) {
            Replacement = FileLink(Key.Mid(5), LinkText);
        } else if (Key.StartsWith(wxT("http:"))) {
            Replacement = TypedLink(Key, LinkText);
        } else if (Key.StartsWith(wxT("https:"))) {
            Replacement = TypedLink(Key, LinkText);
        } else if (Key.StartsWith(wxT("mailto:"))) {
            Replacement = TypedLink(Key, LinkText);
        } else if (Key.StartsWith(wxT("*URL*"))) {
            Replacement = TypedLink(Key, LinkText);
        } else {
            Replacement = InnerLink(Key, LinkText);
        }

        Temp = Temp.Mid(0, i) + Replacement + Temp.Mid(i + j + k + 5);  // 5 for the [[|]]
    }
    return Temp;
}

TranslatableString TitleText(const wxString& Key)
{
    if (Key == wxT("welcome")) {
        return XO("Welcome!");
    }

    if (Key == wxT("play")) {
        /* i18n-hint: Title for a topic.*/
        return XO("Playing Audio");
    }
    if ((Key == wxT("record")) || (Key == wxT("norecord"))) {
        /* i18n-hint: Title for a topic.*/
        return XO("Recording Audio");
    }
    if (Key == wxT("inputdevice")) {
        /* i18n-hint: Title for a topic.*/
        return XO("Recording - Choosing the Recording Device");
    }
    if (Key == wxT("inputsource")) {
        /* i18n-hint: Title for a topic.*/
        return XO("Recording - Choosing the Recording Source");
    }
    if (Key == wxT("inputlevel")) {
        /* i18n-hint: Title for a topic.*/
        return XO("Recording - Setting the Recording Level");
    }
    if ((Key == wxT("edit")) || (Key == wxT("grey"))) {
        /* i18n-hint: Title for a topic.*/
        return XO("Editing and greyed out Menus");
    }
    if (Key == wxT("export")) {
        /* i18n-hint: Title for a topic.*/
        return XO("Exporting an Audio File");
    }
    if (Key == wxT("save")) {
        /* i18n-hint: Title for a topic.*/
        return XO("Saving an Audacity Project");
    }
    if (Key == wxT("wma-proprietary")) {
        /* i18n-hint: Title for a topic.*/
        return XO("Support for Other Formats");
    }
    if (Key == wxT("burncd")) {
        /* i18n-hint: Title for a topic.*/
        return XO("Burn to CD");
    }
    if (Key == wxT("remotehelp")) {
        return XO("No Local Help");
    }
    // Uh oh, no translation...
    return Verbatim(Key);
}

static wxString HelpTextBuiltIn(const wxString& Key)
{
    if (Key == wxT("wma-proprietary")) {
        wxStringOutputStream o;
        wxTextOutputStream s(o);
        s
            << wxT("<p>")
            << XO(
            "Audacity can import unprotected files in many other formats (such as M4A and WMA, \
compressed WAV files from portable recorders and audio from video files) if you download and install \
the optional [[https://support.audacityteam.org/basics/installing-ffmpeg| \
FFmpeg library]] to your computer.")
            << wxT("</p><p>")
            << XO(
            "You can also read our help on importing \
[[https://manual.audacityteam.org/man/playing_and_recording.html#midi|MIDI files]] \
and tracks from [[https://manual.audacityteam.org/man/faq_opening_and_saving_files.html#fromcd| \
audio CDs]].")
            << wxT("</p>")
        ;
        return WrapText(o.GetString());
    }

    // Remote help allows us to link to a local copy of the help if it exists,
    // or provide a message that takes you to the Internet if it does not.
    // It's used by the menu item Help > Index
    if (Key == wxT("remotehelp")) {
        wxStringOutputStream o;
        wxTextOutputStream s(o);
        s
// *URL* will be replaced by whatever URL we are looking for.
            << XO(
            "The Manual does not appear to be installed. \
Please [[*URL*|view the Manual online]] or \
[[https://manual.audacityteam.org/man/unzipping_the_manual.html| \
download the Manual]].<br><br>\
To always view the Manual online, change \"Location of Manual\" in \
Interface Preferences to \"From Internet\".")
        ;
        return WrapText(o.GetString());
    }
    return {};
}

wxString HelpText(const wxString& Key)
{
    // Possible future enhancement...
    // We could look for the text as a local file and use
    // that if we find it...
    // if( wxFileExists( Path+Key ) )
    // ...

    wxString Text;
    Text = HelpTextBuiltIn(Key);

    if (!Text.empty()) {
        return LinkExpand(Text);
    }

    // Perhaps useful for debugging - we'll return key that we didn't find.
    return WrapText(Key);
}

wxString FormatHtmlText(const wxString& Text)
{
    wxString localeStr = wxLocale::GetSystemEncodingName();

    return
        wxT("<html><head><META http-equiv=\"Content-Type\" content=\"text/html; charset=")
        + localeStr
        + wxT("\"></head>")
        + WrapText(LinkExpand(Text))
        + wxT("</html>");
}
